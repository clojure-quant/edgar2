(ns edgar2.core
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar.download :as dl]
            [tech.v3.dataset :as ds])
  (:gen-class))

(def default-ticker "XOM")

;; SEC's company_tickers.json currently maps XOM to ExxonMobil Holdings Corp
;; (CIK 0002115436), a new registrant with no 10-Ks. Annual financials live
;; on EXXON MOBIL CORP (CIK 0000034088).
(def operating-cik "0000034088")

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def filing-dir "data/filings")

(defn resolve-filer
  [ticker]
  (let [mapped (e/cik ticker)
        annuals (e/filings mapped :form "10-K" :limit 1)]
    (if (seq annuals)
      mapped
      operating-cik)))

(defn fye-month
  "Month (\"06\", \"12\") from SEC :fiscal-year-end (\"0630\") or an :end date."
  [fiscal-year-end]
  (let [s (str fiscal-year-end)]
    (cond
      (re-matches #"\d{4}" s) (subs s 0 2)
      (re-find #"-\d{2}-" s) (second (re-find #"-(\d{2})-" s)))))

(defn fiscal-period-end?
  [end fye-mm]
  (let [s (str end)]
    (if fye-mm
      (boolean (re-find (re-pattern (str "-" fye-mm "-")) s))
      true)))

(defn last-n-annual-rows
  [wide-ds n fiscal-year-end]
  (let [ends* (map :end (ds/mapseq-reader wide-ds))
        mm (or (fye-month fiscal-year-end)
               (fye-month (some->> ends* (map str) sort last)))
        ends (->> ends*
                  (filter #(fiscal-period-end? % mm))
                  (map str)
                  distinct
                  sort
                  reverse
                  (take n)
                  set)]
    (ds/filter-column wide-ds :end #(contains? ends (str %)))))

(defn strip-html
  [html]
  (-> (or html "")
      (str/replace #"<[^>]+>" " ")
      (str/replace #"&nbsp;|&#160;" " ")
      (str/replace #"\s+" " ")))

(defn millions-after
  "First comma-grouped integer after `label` in `text`, as dollars."
  [text label]
  (when-let [idx (str/index-of (str/lower-case text) (str/lower-case label))]
    (when-let [m (re-find #"\d{1,3}(?:,\d{3})+" (subs text (+ idx (count label))))]
      (* 1000000 (parse-long (str/replace m "," ""))))))

(defn extract-purchases
  "Crude oil and product purchases from a 10-K — Exxon's COGS equivalent."
  [filing]
  (let [text (strip-html (e/html filing))
        year (when-let [rd (:reportDate filing)]
               (parse-long (subs (str rd) 0 4)))
        purchases (millions-after text "Crude oil and product purchases")
        sales (millions-after text "Sales and other operating revenue")]
    {:year year
     :end (:reportDate filing)
     :sales sales
     :purchases purchases
     :gross-profit (when (and sales purchases) (- sales purchases))}))

(defn ensure-identity!
  []
  (e/init! identity-header))

(defn opts
  "Normalize -X exec args. Accepts :ticker and :years (or :n)."
  [{:keys [ticker n years]
    :or {ticker default-ticker}}]
  {:ticker (str/upper-case (name ticker))
   :n (long (or years n 5))})

(defn download
  "Download the last `:years` annual 10-Ks for `:ticker` into `filing-dir`.

  Usage: clj -X:download :ticker AAPL :years 3"
  ([] (download {}))
  ([m]
   (let [{:keys [ticker n]} (opts m)]
     (ensure-identity!)
     (let [cik (resolve-filer ticker)
           meta (e/company-metadata cik)
           results (dl/download-filings! cik filing-dir
                                         :form "10-K"
                                         :limit n
                                         :skip-existing? true)]
       (println (format "Company: %s  ticker=%s  CIK=%s"
                        (:name meta) ticker cik))
       (when (not= cik (e/cik ticker))
         (println (format "Note: SEC ticker %s maps to %s; using operating filer %s."
                          ticker (e/cik ticker) cik)))
       (println "\n10-K downloads:")
       (doseq [result results]
         (println (format "  %-8s %s"
                          (name (:status result))
                          (or (:path result)
                              (:message result)
                              (:exception result)
                              (pr-str result)))))
       results))))

(defn report
  "Print revenue, derived gross profit, and net profit for the last `:years`.

  Usage: clj -X:report :ticker XOM :years 5"
  ([] (report {}))
  ([m]
   (let [{:keys [ticker n]} (opts m)]
     (ensure-identity!)
     (let [cik (resolve-filer ticker)
           meta (e/company-metadata cik)
           income (last-n-annual-rows
                   (e/income cik :form "10-K" :shape :wide :view :standardized)
                   n
                   (:fiscal-year-end meta))
           by-year (->> (e/filings cik :form "10-K" :limit n)
                        (map extract-purchases)
                        (filter :year)
                        (into {} (map (juxt :year identity))))
           rows (for [row (ds/mapseq-reader income)
                      :let [year (parse-long (subs (str (:end row)) 0 4))
                            extra (get by-year year {})]]
                  {:year year
                   :end (str (:end row))
                   :revenue (get row "Revenue")
                   :gross-profit (or (get row "Gross Profit")
                                     (:gross-profit extra))
                   :net-profit (get row "Net Income")})
           ds (-> (ds/->dataset rows)
                  (ds/select-columns [:year :end :revenue :gross-profit :net-profit])
                  (ds/sort-by-column :year >))]
       (println "\nAnnual financials (USD). Gross profit is derived as")
       (println "Sales and other operating revenue − Crude oil and product purchases")
       (println "(Exxon does not tag GrossProfit or CostOfRevenue in XBRL).")
       (println)
       (println ds)
       (println)
       (println "Same figures in $ millions:")
       (println
        (ds/->dataset
         (for [row (ds/mapseq-reader ds)]
           {:year (:year row)
            :revenue (some-> (:revenue row) (/ 1.0e6) long)
            :gross-profit (some-> (:gross-profit row) (/ 1.0e6) long)
            :net-profit (some-> (:net-profit row) (/ 1.0e6) long)})
         {:dataset-name (str ticker " annual ($ millions)")}))
       ds))))

(defn -main
  [& _]
  (download)
  (report))
