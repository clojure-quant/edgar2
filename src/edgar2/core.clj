(ns edgar2.core
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar.download :as dl]
            [clojure.pprint :as pprint]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate]
           [java.time.temporal ChronoUnit])
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

(defn parse-date
  [d]
  (when d
    (try (LocalDate/parse (str d))
         (catch Exception _ nil))))

(defn period-days
  [start end]
  (when-let [s (parse-date start)]
    (when-let [e (parse-date end)]
      (.between ChronoUnit/DAYS s e))))

(defn annual-duration?
  "True for ~12-month periods. 10-Ks also repeat the last quarter
  (same :end, :fp FY) — those are ~90 days and must be dropped."
  [row]
  (when-let [days (period-days (:start row) (:end row))]
    (<= 300 days 400)))

(defn latest-filed
  [rows]
  (->> rows
       (group-by #(str (:end %)))
       vals
       (map (fn [rs] (apply max-key #(str (:filed %)) rs)))))

(defn annual-statement-rows
  "Full-year income-statement fact rows (quarterly repeats removed)."
  [cik fiscal-year-end]
  (let [rows (ds/mapseq-reader (e/income cik :form "10-K" :view :standardized))
        mm (or (fye-month fiscal-year-end)
               (fye-month (some->> rows (map :end) (map str) sort last)))]
    (->> rows
         (filter annual-duration?)
         (filter #(fiscal-period-end? (:end %) mm)))))

(defn annual-line-values
  "Map of period-end string → value for one income-statement line item."
  [rows line-item]
  (->> rows
       (filter #(= line-item (:line-item %)))
       latest-filed
       (map (juxt #(str (:end %)) :val))
       (into {})))

(defn annual-income
  "Last `n` full-year rows: revenue, cogs, gross profit, net income."
  [cik n fiscal-year-end]
  (let [annual (annual-statement-rows cik fiscal-year-end)
        rev (annual-line-values annual "Revenue")
        cogs (annual-line-values annual "Cost of Revenue")
        gp (annual-line-values annual "Gross Profit")
        ni (annual-line-values annual "Net Income")
        ends (->> (keys rev) sort reverse (take n))]
    (for [end ends]
      {:year (parse-long (subs end 0 4))
       :end end
       :revenue (get rev end)
       :cogs (get cogs end)
       :gross-profit (get gp end)
       :net-profit (get ni end)})))

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
  "Print revenue, cogs, gross profit, and net profit for the last `:years`.

  Usage: clj -X:report :ticker XOM :years 5"
  ([] (report {}))
  ([m]
   (let [{:keys [ticker n]} (opts m)]
     (ensure-identity!)
     (let [cik (resolve-filer ticker)
           meta (e/company-metadata cik)
           base (annual-income cik n (:fiscal-year-end meta))
           need-fallback? (some #(or (nil? (:gross-profit %)) (nil? (:cogs %))) base)
           by-year (if need-fallback?
                     (->> (e/filings cik :form "10-K" :limit n)
                          (map extract-purchases)
                          (filter :year)
                          (into {} (map (juxt :year identity))))
                     {})
           rows (for [row base
                      :let [extra (get by-year (:year row) {})
                            cogs (or (:cogs row) (:purchases extra))
                            gp (or (:gross-profit row) (:gross-profit extra))
                            cogs (or cogs (when (and (:revenue row) gp)
                                            (- (:revenue row) gp)))
                            gp (or gp (when (and (:revenue row) cogs)
                                        (- (:revenue row) cogs)))]]
                  (assoc row :cogs cogs :gross-profit gp))
           ds (ds/->dataset
              (for [row (sort-by :year > rows)]
                {:year (:year row)
                 :end (:end row)
                 :revenue (some-> (:revenue row) (/ 1.0e6) long)
                 :cogs (some-> (:cogs row) (/ 1.0e6) long)
                 :gross-profit (some-> (:gross-profit row) (/ 1.0e6) long)
                 :net-profit (some-> (:net-profit row) (/ 1.0e6) long)})
              {:dataset-name (str ticker " annual ($ millions)")})]
       (when need-fallback?
         (println "COGS / gross profit filled from sales − crude oil and product purchases")
         (println "(this filer does not tag those lines in XBRL)."))
       (println)
       (println ds)
       ds))))

(def pl-column-order
  ["Revenue"
   "Cost of Revenue"
   "Gross Profit"
   "R&D Expense"
   "Selling and Marketing Expense"
   "General and Administrative Expense"
   "SG&A Expense"
   "Operating Expenses"
   "Operating Income"
   "Interest Expense"
   "Non-Operating Income"
   "Pre-Tax Income"
   "Income Tax Expense"
   "Net Income"
   "EPS Basic"
   "EPS Diluted"
   "Shares Basic"
   "Shares Diluted"])

(defn pl-scale
  "USD and share counts → millions. EPS stays in dollars."
  [line-item val]
  (when val
    (if (re-find #"(?i)eps|per share" (str line-item))
      (double val)
      (long (/ val 1.0e6)))))

(defn pl
  "Print every standardized income-statement line for the last `:years`.

  Usage: clj -X:pl :ticker MSFT :years 20"
  ([] (pl {}))
  ([m]
   (let [{:keys [ticker n]} (opts m)]
     (ensure-identity!)
     (let [cik (resolve-filer ticker)
           meta (e/company-metadata cik)
           annual (annual-statement-rows cik (:fiscal-year-end meta))
           items (vec (distinct (map :line-item annual)))
           by-item (into {} (for [item items]
                              [item (annual-line-values annual item)]))
           ends (->> (or (keys (by-item "Revenue"))
                         (map #(str (:end %)) annual))
                     distinct
                     sort
                     reverse
                     (take n)
                     sort)
           year-cols (mapv #(subs (str %) 0 4) ends)
           print-cols (into [:field] year-cols)
           item-order (concat (filter (set items) pl-column-order)
                              (sort (remove (set pl-column-order) items)))
           rows (map (fn [item]
                       (into {:field item}
                             (map (fn [end year]
                                    [year (pl-scale item (get (by-item item) end))])
                                  ends
                                  year-cols)))
                     item-order)
           ds (ds/->dataset rows
                            {:dataset-name (str ticker " P&L ($ millions; EPS in $)")
                             :column-order print-cols})]
       (println)
       (println (str ticker " P&L ($ millions; EPS in $)"))
       (pprint/print-table print-cols rows)
       ds))))

(defn -main
  [& _]
  (download)
  (report))
