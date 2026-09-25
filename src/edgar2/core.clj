(ns edgar2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.download :as dl]
            [clojure.pprint :as pprint]
            [edgar.core :as edgar]
            [edgar.fsds :as fsds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.io BufferedReader InputStreamReader]
           [java.nio.charset StandardCharsets]
           [java.time LocalDate]
           [java.time.temporal ChronoUnit]
           [java.util.zip ZipFile])
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

(defn derived-operating-expenses
  "When filers (esp. E&P) do not tag OperatingExpenses: pretax ≈
  revenue − cogs − opex − interest + other income + asset gains."
  [by-item end]
  (let [g (fn [item] (get (by-item item) end))
        rev (g "Revenue")
        pretax (g "Pre-Tax Income")]
    (when (and rev pretax)
      (- (+ rev
            (or (g "Non-Operating Income") 0)
            (or (g "Gain (Loss) on Sale of Assets") 0))
         pretax
         (or (g "Cost of Revenue") 0)
         (or (g "Interest Expense") 0)))))

(defn with-derived-opex
  [by-item ends]
  (let [existing (or (by-item "Operating Expenses") {})
        filled (reduce (fn [m end]
                         (if (get m end)
                           m
                           (if-let [v (derived-operating-expenses by-item end)]
                             (assoc m end v)
                             m)))
                       existing
                       ends)]
    (assoc by-item "Operating Expenses" filled)))

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
           by-item* (into {} (for [item items]
                               [item (annual-line-values annual item)]))
           ends (->> (or (keys (by-item* "Revenue"))
                         (map #(str (:end %)) annual))
                     distinct
                     sort
                     reverse
                     (take n)
                     sort)
           by-item (with-derived-opex by-item* ends)
           items (cond-> items
                   (seq (by-item "Operating Expenses"))
                   (as-> xs (vec (distinct (conj xs "Operating Expenses")))))
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

(defn pl-fields
  "Print last-year income-statement tags: line, XBRL concept, value.

  Usage: clj -X:pl-fields :ticker OXY"
  ([] (pl-fields {}))
  ([m]
   (let [{:keys [ticker]} (opts m)]
     (ensure-identity!)
     (let [cik (resolve-filer ticker)
           meta (e/company-metadata cik)
           fye (:fiscal-year-end meta)
           reported (ds/mapseq-reader (e/income cik :form "10-K" :view :as-reported))
           mm (or (fye-month fye)
                  (fye-month (some->> reported (map :end) (map str) sort last)))
           annual (->> reported
                       (filter annual-duration?)
                       (filter #(fiscal-period-end? (:end %) mm)))
           latest-end (->> annual (map #(str (:end %))) sort last)
           line-by-concept (->> (annual-statement-rows cik fye)
                                (filter #(= latest-end (str (:end %))))
                                (map (juxt :concept :line-item))
                                (into {}))
           fields (->> annual
                       (filter #(= latest-end (str (:end %))))
                       (group-by :concept)
                       vals
                       (map (fn [rs] (apply max-key #(str (:filed %)) rs)))
                       (map (fn [r]
                              (let [usd? (= "USD" (:unit r))]
                                {:line (or (line-by-concept (:concept r))
                                           (:label r))
                                 :tag (:concept r)
                                 :value (when-let [v (:val r)]
                                          (if usd?
                                            (long (/ v 1.0e6))
                                            v))
                                 :unit (if usd? "USD millions" (:unit r))})))
                       (sort-by (fn [{:keys [line tag]}]
                                  (let [i (.indexOf pl-column-order (str line))]
                                    [(if (neg? i) 1 0) (if (neg? i) 99 i) (str line) (str tag)]))))]
       (println (format "%s  ticker=%s  CIK=%s  period=%s"
                        (:name meta) ticker cik latest-end))
       (println "Income-statement tags (as reported; USD in millions)")
       (println)
       (pprint/print-table [:line :tag :value :unit] fields)
       (ds/->dataset fields
                     {:dataset-name (str ticker " P&L fields " latest-end)
                      :column-order [:line :tag :value :unit]})))))

(def business-start-re
  #"(?i)items?\s+1(?:\s+and\s+2)?[\s.:,—-]+business")

(def business-end-re
  #"(?i)item\s+1a[\s.:,—-]+risk factors")

(defn last-re-start
  [re s]
  (loop [m (re-matcher re s) idx nil]
    (if (.find m)
      (recur m (.start m))
      idx)))

(defn extract-business-text
  "Item 1 Business from the latest 10-K.

  e/item \"1\" often binds to Item 1C (Cybersecurity). Many E&P filers
  also title the section ITEMS 1 AND 2. BUSINESS AND PROPERTIES."
  [filing]
  (let [text (e/text filing)
        start (last-re-start business-start-re text)]
    (when start
      (let [tail (subs text start)
            end-m (re-matcher business-end-re tail)
            body (if (.find end-m)
                   (subs tail 0 (.start end-m))
                   tail)]
        {:title "Item 1. Business"
         :text (str/trim body)
         :method :item-1-heading}))))

(defn business
  "Print Item 1 (Business) from the latest 10-K.

  Usage: clj -X:business :ticker OXY"
  ([] (business {}))
  ([m]
   (let [{:keys [ticker]} (opts m)]
     (ensure-identity!)
     (let [cik (resolve-filer ticker)
           meta (e/company-metadata cik)
           f (e/filing cik :form "10-K")
           item (extract-business-text f)]
       (println (format "%s  ticker=%s  CIK=%s"
                        (:name meta) ticker cik))
       (println (format "10-K  period=%s  filed=%s  accession=%s"
                        (:reportDate f) (:filingDate f) (:accessionNumber f)))
       (when-let [url (:url f)]
         (println url))
       (println)
       (if item
         (do
           (println (:title item))
           (println)
           (println (:text item)))
         (println "Could not extract Item 1 (Business) from this 10-K."))
       item))))

(def tickers-url
  "https://www.sec.gov/files/company_tickers.json")

(def filers-path "data/filers.csv")

(defn pad-cik
  [cik]
  (format "%010d" (Long/parseLong (str cik))))

(defn fetch-filers
  "SEC company_tickers.json → seq of {:ticker :cik :name}."
  []
  (let [client (java.net.http.HttpClient/newHttpClient)
        req (-> (java.net.http.HttpRequest/newBuilder)
                (.uri (java.net.URI/create tickers-url))
                (.header "User-Agent" identity-header)
                (.header "Accept" "application/json")
                (.GET)
                (.build))
        resp (.send client req (java.net.http.HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (when-not (= 200 status)
      (throw (ex-info (str "SEC tickers fetch failed: HTTP " status)
                      {:status status :body (.body resp)})))
    (->> (json/read-value (.body resp) json/keyword-keys-object-mapper)
         vals
         (map (fn [{:keys [ticker cik_str title]}]
                {:ticker ticker
                 :cik (pad-cik cik_str)
                 :name title}))
         (sort-by :ticker))))

(defn ticker-rank
  "Lower rank is closer to common stock (no hyphen, shorter symbol)."
  [ticker]
  [(if (str/includes? (str ticker) "-") 1 0)
   (count (str ticker))
   (str ticker)])

(defn filter-primary-filers
  "Keep the main ticker for each CIK (drop units, warrants, preferred)."
  [filers]
  (->> filers
       (group-by :cik)
       vals
       (map (fn [rows] (first (sort-by (comp ticker-rank :ticker) rows))))
       (sort-by :ticker)))

(defn filers
  "Print and save the SEC ticker/CIK directory (one main ticker per CIK).

  Usage: clj -X:filers"
  ([] (filers {}))
  ([_]
   (ensure-identity!)
   (let [all (vec (fetch-filers))
         rows (vec (filter-primary-filers all))
         ds (ds/->dataset rows
                          {:dataset-name "SEC filers (company_tickers.json)"
                           :column-order [:ticker :cik :name]})]
     (.mkdirs (java.io.File. "data"))
     (ds/write! ds filers-path)
     (println (format "SEC listed filers: %d tickers → %d primary (from %s)"
                      (count all) (count rows) tickers-url))
     (println (format "Wrote %s" filers-path))
     (println)
     (pprint/print-table [:ticker :cik :name] (take 25 rows))
     (println (format "... %d more (see %s)" (max 0 (- (count rows) 25)) filers-path))
     ds)))

(def filer-info-path "data/filer-info.edn")
(def fsds-dir "data/fsds")
(def exchange-url "https://www.sec.gov/files/company_tickers_exchange.json")

(def revenue-tags
  ["RevenueFromContractWithCustomerExcludingAssessedTax"
   "Revenues"
   "SalesRevenueNet"
   "SalesRevenueGoodsNet"
   "RevenueFromContractWithCustomerIncludingAssessedTax"
   "RevenuesNetOfInterestExpense"
   "InterestAndDividendIncomeOperating"
   "PremiumsEarnedNet"
   "RealEstateRevenueNet"])

(def employee-tags ["EntityNumberOfEmployees"])
(def float-tags ["EntityPublicFloat"])
(def shares-tags ["EntityCommonStockSharesOutstanding"
                  "CommonStockSharesOutstanding"
                  "WeightedAverageNumberOfSharesOutstandingBasic"])

(def overview-tags
  (set (concat revenue-tags employee-tags float-tags shares-tags)))

(def afs-labels
  {"LAF" "Large accelerated filer"
   "1-LAF" "Large accelerated filer"
   "ACC" "Accelerated filer"
   "2-ACC" "Accelerated filer"
   "SRA" "Smaller reporting accelerated filer"
   "3-SRA" "Smaller reporting accelerated filer"
   "NON" "Non-accelerated filer"
   "4-NON" "Non-accelerated filer"
   "SML" "Smaller reporting company"
   "5-SML" "Smaller reporting company"})

(def sic-major-groups
  {"01" "Agricultural production — crops"
   "02" "Agricultural production — livestock"
   "07" "Agricultural services"
   "08" "Forestry"
   "09" "Fishing, hunting, and trapping"
   "10" "Metal mining"
   "12" "Coal mining"
   "13" "Oil and gas extraction"
   "14" "Nonmetallic minerals"
   "15" "Building construction"
   "16" "Heavy construction"
   "17" "Special trade contractors"
   "20" "Food and kindred products"
   "21" "Tobacco"
   "22" "Textile mill products"
   "23" "Apparel"
   "24" "Lumber and wood"
   "25" "Furniture"
   "26" "Paper"
   "27" "Printing and publishing"
   "28" "Chemicals"
   "29" "Petroleum refining"
   "30" "Rubber and plastics"
   "31" "Leather"
   "32" "Stone, clay, glass, concrete"
   "33" "Primary metals"
   "34" "Fabricated metals"
   "35" "Industrial machinery and computers"
   "36" "Electronic and electrical equipment"
   "37" "Transportation equipment"
   "38" "Instruments"
   "39" "Miscellaneous manufacturing"
   "40" "Railroad transportation"
   "41" "Local transit"
   "42" "Motor freight"
   "43" "US Postal Service"
   "44" "Water transportation"
   "45" "Air transportation"
   "46" "Pipelines (except natural gas)"
   "47" "Transportation services"
   "48" "Communications"
   "49" "Electric, gas, and sanitary services"
   "50" "Wholesale — durable"
   "51" "Wholesale — nondurable"
   "52" "Building materials and garden"
   "53" "General merchandise stores"
   "54" "Food stores"
   "55" "Auto dealers and gas stations"
   "56" "Apparel stores"
   "57" "Home furniture stores"
   "58" "Eating and drinking places"
   "59" "Miscellaneous retail"
   "60" "Depository institutions"
   "61" "Non-depository credit"
   "62" "Security and commodity brokers"
   "63" "Insurance carriers"
   "64" "Insurance agents and brokers"
   "65" "Real estate"
   "67" "Holding and investment offices"
   "70" "Hotels and lodging"
   "72" "Personal services"
   "73" "Business services"
   "75" "Automotive repair and services"
   "76" "Miscellaneous repair"
   "78" "Motion pictures"
   "79" "Amusement and recreation"
   "80" "Health services"
   "81" "Legal services"
   "82" "Educational services"
   "83" "Social services"
   "84" "Museums and gardens"
   "86" "Membership organizations"
   "87" "Engineering, accounting, research"
   "88" "Private households"
   "89" "Services, nec"
   "91" "Executive and general government"
   "92" "Justice and public order"
   "93" "Public finance"
   "94" "Human resource programs"
   "95" "Environmental and housing programs"
   "96" "Economic programs"
   "97" "National security"
   "99" "Nonclassifiable"})

(defn load-primary-filers
  "Rows from data/filers.csv, or a fresh SEC download if that file is missing."
  []
  (if (.exists (io/file filers-path))
    (->> (ds/mapseq-reader (ds/->dataset filers-path))
         (map (fn [r]
                (let [g (fn [k] (or (get r k) (get r (name k))))]
                  {:ticker (str (g :ticker))
                   :cik (pad-cik (g :cik))
                   :name (str (g :name))})))
         vec)
    (vec (filter-primary-filers (fetch-filers)))))

(defn save-filer-info!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str filer-info-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file filer-info-path))))

(defn fmt-revenue
  [v]
  (if v
    (format "%.0fM" (/ (double v) 1.0e6))
    "-"))

(defn parse-num
  [s]
  (when (and s (not (str/blank? (str s))))
    (try (Double/parseDouble (str s))
         (catch Exception _ nil))))

(defn yyyymmdd->iso
  [s]
  (let [d (re-find #"\d{8}" (str s))]
    (when d
      (str (subs d 0 4) "-" (subs d 4 6) "-" (subs d 6 8)))))

(defn pad-fye
  [s]
  (when (and s (not (str/blank? (str s))))
    (format "%04d" (long (parse-num s)))))

(defn sic-code
  [s]
  (when-let [n (parse-num s)]
    (format "%04d" (long n))))

(defn sic-description
  [sic]
  (when sic
    (get sic-major-groups (subs (str sic) 0 2))))

(defn category-label
  [afs]
  (when (and afs (not (str/blank? (str afs))))
    (or (afs-labels (str/upper-case (str afs)))
        (afs-labels (str/upper-case (last (str/split (str afs) #"-"))))
        (str afs))))

(defn recent-quarters
  "Calendar quarters newest-first, starting at the current quarter."
  [n]
  (let [today (LocalDate/now)]
    (loop [y (.getYear today)
           q (inc (quot (dec (.getMonthValue today)) 3))
           acc []]
      (if (>= (count acc) n)
        acc
        (recur (if (= q 1) (dec y) y)
               (if (= q 1) 4 (dec q))
               (conj acc [y q]))))))

(defn download-fsds-quarter
  [year quarter]
  (let [path (str (io/file fsds-dir (str year "q" quarter ".zip")))]
    (if (.exists (io/file path))
      (do (println (format "  have %dq%d" year quarter))
          path)
      (try
        (println (format "  download %dq%d …" year quarter))
        (flush)
        (fsds/download-quarter! year quarter fsds-dir)
        (catch Exception ex
          (println (format "  skip %dq%d (%s)" year quarter (.getMessage ex)))
          nil)))))

(defn reduce-zip-tsv
  [zip-path entry-name rf init]
  (with-open [zf (ZipFile. (str zip-path))]
    (let [entry (.getEntry zf entry-name)]
      (when-not entry
        (throw (ex-info (str entry-name " missing in " zip-path)
                        {:zip zip-path :entry entry-name})))
      (with-open [r (BufferedReader.
                     (InputStreamReader. (.getInputStream zf entry)
                                         StandardCharsets/UTF_8))]
        (let [headers (mapv keyword (str/split (str/trimr (.readLine r)) #"\t"))]
          (loop [acc init]
            (if-let [line (.readLine r)]
              (recur (rf acc (zipmap headers (str/split (str/trimr line) #"\t" -1))))
              acc)))))))

(defn newer-sub?
  [a b]
  (pos? (compare (str (:filed a)) (str (:filed b)))))

(defn collect-10k-subs
  "Latest 10-K submission per CIK from FSDS SUB tables."
  [zip-paths]
  (reduce
   (fn [by-cik zip-path]
     (println (format "  parse SUB %s" (.getName (io/file zip-path))))
     (flush)
     (reduce-zip-tsv
      zip-path "sub.txt"
      (fn [acc row]
        (if (or (not= "10-K" (str (:form row)))
                (str/blank? (str (:cik row))))
          acc
          (let [cik (pad-cik (:cik row))
                sub {:cik cik
                     :adsh (:adsh row)
                     :sic (sic-code (:sic row))
                     :afs (:afs row)
                     :fye (pad-fye (:fye row))
                     :period (str (:period row))
                     :filed (str (:filed row))}]
            (if (or (nil? (get acc cik)) (newer-sub? sub (get acc cik)))
              (assoc acc cik sub)
              acc))))
      by-cik))
   {}
   zip-paths))

(defn better-fact
  [prev row period]
  (let [ddate (str (:ddate row))
        qtrs (or (some-> (:qtrs row) parse-num long) -1)
        val (parse-num (:value row))
        cand {:val val :ddate ddate :qtrs qtrs}
        period (str period)]
    (cond
      (nil? val) prev
      (nil? prev) cand
      (and (= ddate period) (not= (:ddate prev) period)) cand
      (and (= ddate period) (= (:ddate prev) period) (> qtrs (:qtrs prev))) cand
      (and (not= (:ddate prev) period) (pos? (compare ddate (:ddate prev)))) cand
      :else prev)))

(defn collect-overview-facts
  "NUM facts for the latest-10-K accessions only."
  [zip-paths subs-by-cik]
  (let [period-by-adsh (into {} (map (juxt :adsh :period) (vals subs-by-cik)))
        wanted (set (keys period-by-adsh))]
    (reduce
     (fn [facts zip-path]
       (println (format "  parse NUM %s" (.getName (io/file zip-path))))
       (flush)
       (reduce-zip-tsv
        zip-path "num.txt"
        (fn [acc row]
          (let [adsh (:adsh row)
                tag (:tag row)]
            (if (or (not (wanted adsh))
                    (not (overview-tags tag))
                    (not (str/blank? (str (:coreg row))))
                    (not (str/blank? (str (:segments row)))))
              acc
              (update-in acc [adsh tag]
                         better-fact row (period-by-adsh adsh)))))
        facts))
     {}
     zip-paths)))

(defn fact-val
  [facts adsh tags qtrs]
  (some (fn [tag]
          (let [f (get-in facts [adsh tag])]
            (when (and f (or (nil? qtrs) (= qtrs (:qtrs f))))
              (:val f))))
        tags))

(defn fact-val*
  [facts adsh tags qtrs]
  (or (fact-val facts adsh tags qtrs)
      (fact-val facts adsh tags nil)))

(defn fetch-exchanges
  "ticker → exchange from company_tickers_exchange.json (one request)."
  []
  (let [payload (edgar/edgar-get exchange-url)
        fields (mapv keyword (:fields payload))]
    (into {}
          (for [row (:data payload)
                :let [m (zipmap fields row)]]
            [(str/upper-case (str (:ticker m))) (:exchange m)]))))

(defn build-filer-info-row
  [filer sub facts exchange]
  (let [adsh (:adsh sub)
        sic (:sic sub)]
    {:ticker (:ticker filer)
     :cik (:cik filer)
     :name (:name filer)
     :exchange exchange
     :sic sic
     :sic-description (sic-description sic)
     :category (category-label (:afs sub))
     :fiscal-year-end (:fye sub)
     :last-10k-date (yyyymmdd->iso (:filed sub))
     :last-10k-period (yyyymmdd->iso (:period sub))
     :last-10k-accession adsh
     :last-10k-revenue (fact-val* facts adsh revenue-tags 4)
     :employees (some-> (fact-val* facts adsh employee-tags 0) long)
     :public-float (fact-val* facts adsh float-tags 0)
     :shares-outstanding (some-> (fact-val* facts adsh shares-tags 0) long)}))

(defn filer-info
  "Build an overview of all primary filers from a few SEC bulk files
  (FSDS quarters + company_tickers_exchange.json), not one API call per CIK.

  Per-company 10-K HTML / full statements stay on -X:report, -X:pl, -X:business.

  Usage: clj -X:filer-info
         clj -X:filer-info :limit 10"
  ([] (filer-info {}))
  ([{:keys [limit]}]
   (ensure-identity!)
   (e/enable-disk-cache! :dir "data/edgar-cache")
   (let [all (load-primary-filers)
         filers (vec (cond->> all limit (take (long limit))))
         total (count all)
         n (count filers)]
     (println (format "filer-info  overview from FSDS bulk files  %d filers  → %s"
                      n filer-info-path))
     (println "Downloading recent FSDS quarters (cached after the first run)…")
     (let [zips (loop [qs (recent-quarters 8) acc []]
                  (cond
                    (or (>= (count acc) 4) (empty? qs)) acc
                    :else
                    (let [path (apply download-fsds-quarter (first qs))]
                      (recur (rest qs) (cond-> acc path (conj path))))))]
       (when (empty? zips)
         (throw (ex-info "No FSDS quarter zips available" {:dir fsds-dir})))
       (println (format "Using %d quarter zip(s)" (count zips)))
       (let [subs (collect-10k-subs zips)
             facts (collect-overview-facts zips subs)
             exchanges (do (println "  fetch company_tickers_exchange.json")
                           (flush)
                           (fetch-exchanges))
             rows (mapv (fn [filer]
                          (build-filer-info-row
                           filer
                           (get subs (:cik filer))
                           facts
                           (get exchanges (:ticker filer))))
                        filers)]
         (doseq [[i row] (map-indexed vector rows)]
           (println (format "%d/%d  %s  %s  %s  10-K=%s  period=%s  rev=%s  sic=%s  %s  shares=%s  emp=%s"
                            (inc i) (or limit total)
                            (:ticker row) (:cik row) (:name row)
                            (or (:last-10k-date row) "-")
                            (or (:last-10k-period row) "-")
                            (fmt-revenue (:last-10k-revenue row))
                            (or (:sic row) "-")
                            (or (:exchange row) "-")
                            (or (:shares-outstanding row) "-")
                            (or (:employees row) "-")))
           (flush))
         (save-filer-info! rows)
         (when (.exists (io/file "data/filer-info.log.edn"))
           (io/delete-file "data/filer-info.log.edn" true))
         (println (format "Wrote %s  (%d filers, %d with a 10-K in the FSDS window)"
                          filer-info-path
                          (count rows)
                          (count (filter :last-10k-date rows))))
         rows)))))

(defn -main
  [& _]
  (download)
  (report))
