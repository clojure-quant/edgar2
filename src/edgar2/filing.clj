(ns edgar2.filing
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar.download :as dl]
            [edgar2.concepts :as concepts]
            [edgar2.derived :as derived]
            [clojure.pprint :as pprint]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate]
           [java.time.temporal ChronoUnit])
  (:gen-class))

(def default-ticker "XOM")

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def filing-dir "data/filings")

(def annual-form-names
  ["10-K" "20-F" "40-F"])

(def periodic-form-names
  #{"10-K" "10-Q" "20-F" "40-F" "10-K/A" "10-Q/A" "20-F/A" "40-F/A"})

(defn annual-form
  "10-K, or 20-F / 40-F when that is the annual report."
  [cik]
  (or (some (fn [form]
              (when (seq (e/filings cik :form form :limit 1))
                form))
            annual-form-names)
      "10-K"))

(defn- has-annual-filing?
  [cik]
  (boolean (some #(seq (e/filings cik :form % :limit 1)) annual-form-names)))

(defn- accession-filer-cik
  "Leading CIK in an accession number (NNNNNNNNNN-YY-######)."
  [accession]
  (when-let [m (re-find #"^(\d+)-" (str accession))]
    (format "%010d" (Long/parseLong (second m)))))

(defn- operating-cik-from-submissions
  "When `mapped` is a shell/successor CIK with no annual report, find the
  operating filer from accession prefixes on periodic forms listed on its
  submissions (e.g. XOM → Holdings 0002115436 → EXXON MOBIL 0000034088)."
  [mapped]
  (->> (e/filings mapped :limit 100)
       (filter #(contains? periodic-form-names (:form %)))
       (keep #(accession-filer-cik (:accessionNumber %)))
       (remove #{mapped})
       distinct
       (some #(when (has-annual-filing? %) %))))

(defn resolve-filer
  "CIK to use for financials. Prefer the ticker's mapped CIK when it has a
  10-K/20-F/40-F; otherwise follow accession prefixes on its submissions to
  the operating registrant that actually files annual reports."
  [ticker]
  (let [mapped (e/cik ticker)]
    (cond
      (has-annual-filing? mapped) mapped
      :else (or (operating-cik-from-submissions mapped) mapped))))

;; SEC company_tickers maps XOM → ExxonMobil Holdings (0002115436), not CORP.
(def ^:private xom-operating-cik "0000034088")

(defn resolve-filer2
  "CIK to use for financials, with an explicit XOM override.

  Returns EXXON MOBIL CORP (`0000034088`) for XOM; every other ticker stays
  on `e/cik` — no annual-filing probe and no accession-prefix walk.

  Why the XOM hack is enough (and correct today)
  ----------------------------------------------
  `e/cik` / company_tickers.json binds each symbol to one registrant CIK.
  After Exxon’s holding-company reorganization, that file maps **XOM** to
  **ExxonMobil Holdings Corp** (`0002115436`): a new Exchange Act registrant
  that owns the operating company and carries the listed ticker, but does
  **not** file the historical 10-K series. Annual (and most XBRL) financials
  still live on **EXXON MOBIL CORP** (`0000034088`).

  So `(e/facts \"XOM\")` hits Holdings and yields empty or thin annual
  periods; `(e/facts \"0000034088\")` is the operating companyfacts the
  screens need.

  A universe scan of ~7.8k primary common tickers found that **only XOM**
  currently needs this remap: every other listed CIK either already has
  annual filings on the mapped registrant, or is a SPAC/fund/new listing
  with no operating successor that carries 10-Ks. Accession-prefix discovery
  (`operating-cik-from-submissions` / `resolve-filer`) generalizes the same
  Holdings→CORP link for XOM, but adds SEC round-trips for a case that is
  unique in the listing file. Hard-coding CORP for XOM is therefore a
  deliberate, documented exception — revisit if SEC retargets the ticker or
  another succession shows up the same way."
  [ticker]
  (if (= (str/upper-case (str ticker)) "XOM")
    xom-operating-cik
    (e/cik ticker)))

(defn fye-month
  "Month (\"06\", \"12\") from SEC :fiscal-year-end (\"0630\") or an :end date."
  [fiscal-year-end]
  (let [s (str fiscal-year-end)]
    (cond
      (re-matches #"\d{4}" s) (subs s 0 2)
      (re-find #"-\d{2}-" s) (second (re-find #"-(\d{2})-" s)))))

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

(defn window-months
  "3, 9, or 12 for a duration that is one quarter, nine months, or a year."
  [row]
  (when-let [days (period-days (:start row) (:end row))]
    (cond
      (<= 75 days 115) 3
      (<= 250 days 290) 9
      (<= 300 days 400) 12)))

(defn fiscal-quarter-label
  "Column name such as \"2025 Q1\" from a period-end and fiscal-year-end month."
  [end fye-mm]
  (if-let [e (parse-date end)]
    (let [month (.getMonthValue e)
          year (.getYear e)
          mm (parse-long (or fye-mm (format "%02d" month)))
          offset (mod (- month mm) 12)
          q (let [k (quot offset 3)] (if (zero? k) 4 k))
          fy (if (> month mm) (inc year) year)]
      (format "%d Q%d" fy q))
    (str end)))

(defn- latest-by-filed
  "Row with the latest :filed. ISO dates compare as strings; `max-key`
  only accepts numbers, so two facts for one period would throw."
  [rows]
  (last (sort-by #(str (:filed %)) rows)))

(defn latest-filed
  [rows]
  (->> rows
       (group-by #(str (:end %)))
       vals
       (map latest-by-filed)))

(defn- concept-local-names
  "edgarjure statement :concepts chains are [[label string ...] ...] —
  schema ConceptChains is strings only, not [taxonomy tag] vectors."
  [pairs]
  (mapv (comp name second) pairs))

(defn- concept-names
  [id]
  (concept-local-names (:data (concepts/get-concept-id id))))

(defn- concept-names-by-label
  [id label]
  (some-> (concepts/get-concept-id id) :data (get label) concept-local-names))

(defn- concept-chain
  "One edgarjure chain: [label concept-string ...]."
  [id]
  (let [{:keys [label data]} (concepts/get-concept-id id)]
    (into [label] (concept-local-names data))))

(def revenue-total-first
  "Headline totals before ASC 606 contract subsets.
  REITs often tag Revenues (or RealEstateRevenueNet) as the full top line
  while also filing a much smaller RevenueFromContractWithCustomer* amount."
  (concept-names :revenue))

(defn- prioritize-revenue-concepts
  "Move known total-revenue tags ahead of contract subsets; keep other order."
  [concepts]
  (let [pri-set (set revenue-total-first)
        preferred (filterv (set concepts) revenue-total-first)
        other (into [] (remove pri-set) concepts)]
    (into preferred other)))

(def depreciation-line
  "Depreciation sits with the operating-expense addends. Income-statement
  tags first; the cash-flow total is the last fallback."
  (concept-chain :depreciation))

(defn income-industry
  "Same SIC routing edgarjure uses for income-statement concept chains."
  [sic]
  (let [n (try (Long/parseLong (str sic)) (catch Exception _ nil))]
    (cond
      (nil? n) :standard
      (or (<= 6000 n 6199) (= n 6712)) :bank
      (or (<= 6300 n 6399) (= n 6411)) :insurance
      (or (= n 6798) (<= 6500 n 6553)) :reit
      :else :standard)))

(defn income-concepts
  "edgarjure income chains with IFRS fallbacks on the existing line labels."
  [cik]
  (let [sic (:sic (e/company-metadata cik))
        chains (:chains (e/concepts-for :income :industry (income-industry sic)))]
    (conj (mapv (fn [[label & concepts]]
                  (let [concepts (cond-> (vec (concat concepts
                                                      (concept-names-by-label :ifrs-income-by-label label)
                                                      (concept-names-by-label :income-fallbacks-by-label label)))
                                   (#{"Revenue" "Total Revenue"} label)
                                   prioritize-revenue-concepts)]
                    (into [label] concepts)))
                chains)
          depreciation-line)))

(defn annual-statement-rows
  "Full-year income-statement fact rows (quarterly repeats removed)."
  ([cik fiscal-year-end]
   (annual-statement-rows cik fiscal-year-end "10-K"))
  ([cik _fiscal-year-end form]
   (->> (ds/mapseq-reader
         (e/income cik :form form :view :standardized
                   :concepts (income-concepts cik)))
        (filter annual-duration?))))

(defn annual-balance-rows
  "Year-end balance-sheet fact rows (instant FY / annual form)."
  [cik _fiscal-year-end form]
  (let [rows (ds/mapseq-reader (e/balance cik :form form :view :standardized))
        forms #{"10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"}]
    (->> rows
         (filter #(or (= "FY" (:fp %))
                      (forms (str (:form %))))))))

(def dividends-paid-label (:label (concepts/get-concept-id :dividends-paid)))
(def stock-repurchase-label (:label (concepts/get-concept-id :stock-repurchase)))

(def cashflow-extra-concepts
  "Dividends and stock repurchase lines pulled with the cash-flow statement.
  edgarjure requires string concept names in each chain."
  [(concept-chain :dividends-paid)
   (concept-chain :stock-repurchase)])

(defn annual-cashflow-rows
  "Full-year cash-flow fact rows (quarterly repeats removed)."
  [cik _fiscal-year-end form concepts]
  (->> (ds/mapseq-reader
        (e/cashflow cik :form form :view :standardized :concepts concepts))
       (filter annual-duration?)))

(defn annual-line-values
  "Map of period-end string → value for one statement line item."
  [rows line-item]
  (->> rows
       (filter #(= line-item (:line-item %)))
       latest-filed
       (map (juxt #(str (:end %)) :val))
       (into {})))

(defn latest-by-start
  [rows]
  (->> rows
       (group-by #(str (:start %)))
       vals
       (map latest-by-filed)))

(defn per-share-line?
  "Share counts and per-share amounts are not additive across quarters."
  [line-item]
  (boolean (re-find #"(?i)eps|per share|shares" (str line-item))))

(defn quarterly-values
  "Period-end → single-quarter amount for one line.

  A reported ~3-month fact wins. Otherwise the quarter is the :val-q on the
  year-to-date row. Q4 is not on the 10-Q; for additive lines it is the
  annual fact minus the nine-month fact with the same start. Share counts
  and EPS are not subtracted."
  [flow-rows annual-rows line-item]
  (let [flow (filter #(= line-item (:line-item %)) flow-rows)
        ann (filter #(= line-item (:line-item %)) annual-rows)
        share? (per-share-line? line-item)
        direct (->> (concat flow ann)
                    (filter #(and (= 3 (window-months %)) (number? (:val %))))
                    latest-filed)
        derived (when-not share?
                  (->> flow
                       (filter #(and (number? (:val-q %))
                                     (not= 3 (window-months %))))
                       (group-by #(str (:end %)))
                       vals
                       (map latest-by-filed)
                       (map (fn [r]
                              {:line-item line-item
                               :end (:end r)
                               :filed (:filed r)
                               :val (:val-q r)}))))
        nine-by-start (->> flow
                           (filter #(= 9 (window-months %)))
                           latest-by-start
                           (map (juxt #(str (:start %)) :val))
                           (into {}))
        q4 (when-not share?
             (keep (fn [row]
                     (when-let [ytd (get nine-by-start (str (:start row)))]
                       (when (and (= 12 (window-months row))
                                  (number? (:val row))
                                  (number? ytd))
                         {:line-item line-item
                          :end (:end row)
                          :filed (:filed row)
                          :val (- (double (:val row)) (double ytd))})))
                   ann))]
    (merge (annual-line-values (concat derived q4) line-item)
           (annual-line-values direct line-item))))

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

(defn parse-period
  ":annual or :quarterly. Missing or unknown values are annual."
  [period]
  (let [s (str/lower-case
           (cond
             (nil? period) "annual"
             (keyword? period) (name period)
             :else (str period)))]
    (if (#{"quarterly" "q"} s) :quarterly :annual)))

(defn opts
  "Normalize -X exec args. Accepts :ticker, :years (or :n), and :period.
  :period is annual (default) or quarterly."
  [{:keys [ticker n years period]
    :or {ticker default-ticker}}]
  {:ticker (str/upper-case (name ticker))
   :n (long (or years n 5))
   :period (parse-period period)})

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
  ["Total Revenue"
   "Revenue"
   "Cost of Revenue"
   "Gross Profit"
   "SG&A Expense"
   "Selling and Marketing Expense"
   "General and Administrative Expense"
   "R&D Expense"
   "Depreciation"
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
   "Shares Diluted"
   "Dividends Paid"
   "Stock Repurchase"])

(def bs-column-order
  "Main balance-sheet lines only."
  ["Cash and Equivalents"
   "Accounts Receivable"
   "Inventory"
   "Current Assets"
   "PP&E Net"
   "Total Assets"
   "Accounts Payable"
   "Current Debt"
   "Current Portion of Long-Term Debt"
   "Short-Term Borrowings"
   "Current Liabilities"
   "Long-Term Debt"
   "Total Liabilities"
   "Stockholders Equity"
   "Total Equity"
   "Total Liabilities and Equity"])

(defn pl-scale
  "USD and share counts → millions. EPS stays in dollars."
  [line-item val]
  (when val
    (if (re-find #"(?i)eps|per share" (str line-item))
      (double val)
      (long (/ val 1.0e6)))))

(defn field-row
  [item by-item ends year-cols]
  (into {:field item}
        (map (fn [end year]
               [year (pl-scale item (get (by-item item) end))])
             ends
             year-cols)))

(defn insert-after
  "Place `row` immediately after the row whose :field is `field`.
  Append when that field is missing. Leave rows unchanged when `row` is nil."
  [rows field row]
  (if-not row
    (vec rows)
    (let [idx (first (keep-indexed (fn [i r] (when (= field (:field r)) i)) rows))]
      (if idx
        (vec (concat (take (inc idx) rows) [row] (drop (inc idx) rows)))
        (vec (concat rows [row]))))))

(defn filing-dates-by-end
  "Map of report period-end → :filingDate from company filings.
  When several filings share an end (e.g. an amendment), keeps the earliest date."
  [cik forms]
  (->> forms
       (mapcat (fn [form] (e/filings cik :form form)))
       (filter #(and (:reportDate %) (:filingDate %)))
       (group-by #(str (:reportDate %)))
       (into {}
             (map (fn [[end xs]]
                    [end (->> xs
                              (map #(str (:filingDate %)))
                              sort
                              first)])))))

(defn statement-rows
  "P&L, a blank line, then the balance sheet. `col-labels` names each period in `ends`.
  `filing-dates` maps period-end string → :filingDate."
  [by-pl* by-bs dividends repurchases ends col-labels filing-dates]
  (let [by-pl (derived/with-derived-lines by-pl* ends)
        items (cond-> (vec (keys by-pl*))
                (seq (by-pl derived/operating-expenses))
                (as-> xs (vec (distinct (conj xs derived/operating-expenses))))
                (seq (by-pl derived/operating-income))
                (as-> xs (vec (distinct (conj xs derived/operating-income))))
                true
                (as-> xs (vec (distinct (conj xs "Depreciation")))))
        columns (into [:field] col-labels)
        year-end (into {:field "year-end"}
                       (map (fn [end label] [label (str end)]) ends col-labels))
        filing-date (into {:field "filing-date"}
                          (map (fn [end label]
                                 [label (get filing-dates (str end))])
                               ends col-labels))
        pl-items (concat (filter (set items) pl-column-order)
                         (sort (remove (set pl-column-order) items)))
        bs-items (filter (fn [item]
                           (some #(get (by-bs item) %) ends))
                         bs-column-order)
        blank (into {:field ""} (map (fn [label] [label nil]) col-labels))
        div-row (when (some #(get dividends %) ends)
                  (field-row dividends-paid-label
                             {dividends-paid-label dividends}
                             ends col-labels))
        repurchase-row (when (some #(get repurchases %) ends)
                         (field-row stock-repurchase-label
                                    {stock-repurchase-label repurchases}
                                    ends col-labels))
        cash-anchor (if div-row dividends-paid-label "Shares Diluted")
        pl-rows (-> (mapv #(field-row % by-pl ends col-labels) pl-items)
                    (insert-after "Shares Diluted" div-row)
                    (insert-after cash-anchor repurchase-row))
        rows (vec (concat [year-end filing-date]
                          pl-rows
                          [blank]
                          (map #(field-row % by-bs ends col-labels) bs-items)))]
    {:columns columns :rows rows}))

(defn period-ends
  [preferred fallback n]
  (->> (or (seq (keys preferred))
           (keys fallback)
           [])
       distinct
       sort
       reverse
       (take n)
       sort
       vec))

(defn column-labels
  "One label per end. Annual labels are the period-end date. Quarterly labels are \"2025 Q1\"."
  [ends period fye-mm]
  (if-not (= period :quarterly)
    (mapv str ends)
    (let [labels (map #(fiscal-quarter-label % fye-mm) ends)
          counts (frequencies labels)]
      (mapv (fn [end label]
              (if (> (get counts label) 1)
                (str label " " end)
                label))
            ends
            labels))))

(defn quarterly-statements
  "10-Q single quarters, with Q4 taken from the annual report minus nine months."
  [cik fiscal-year-end form]
  (let [income (ds/mapseq-reader
                (e/income cik :form "10-Q" :view :standardized
                          :concepts (income-concepts cik)))
        annual (annual-statement-rows cik fiscal-year-end form)
        bs (concat (ds/mapseq-reader (e/balance cik :form "10-Q" :view :standardized))
                   (annual-balance-rows cik fiscal-year-end form))
        cash (ds/mapseq-reader
              (e/cashflow cik :form "10-Q" :view :standardized
                          :concepts cashflow-extra-concepts))
        annual-cash (annual-cashflow-rows cik fiscal-year-end form cashflow-extra-concepts)
        items (vec (distinct (map :line-item (concat income annual))))]
    {:income-values (into {} (for [item items]
                               [item (quarterly-values income annual item)]))
     :balance-values (into {} (for [item (distinct (map :line-item bs))]
                                [item (annual-line-values bs item)]))
     :dividends (quarterly-values cash annual-cash dividends-paid-label)
     :repurchases (quarterly-values cash annual-cash stock-repurchase-label)}))

(defn- ix-attr
  [attrs k]
  (second (re-find (re-pattern (str k "=\"([^\"]*)\"")) (str attrs))))

(defn- xbrl-contexts
  "Inline-XBRL context id → {:member :start :end}. :member is the share
  class on StatementClassOfStockAxis, or nil when the fact is unscoped."
  [html]
  (into {}
        (for [[_ id body] (re-seq #"(?s)<xbrli:context id=\"([^\"]+)\">(.*?)</xbrli:context>"
                                  (str html))]
          [id {:member (some-> (re-find #"StatementClassOfStockAxis\">(?:[^<:\"]+:)?([^<]+)" body)
                               second)
               :start (second (re-find #"<xbrli:startDate>([^<]+)" body))
               :end (or (second (re-find #"<xbrli:endDate>([^<]+)" body))
                        (second (re-find #"<xbrli:instant>([^<]+)" body)))}])))

(defn- visible-before
  "Visible text immediately before `idx`, with tags removed."
  [html idx]
  (let [s (str html)
        from (max 0 (- idx 8000))]
    (-> (subs s from idx)
        (str/replace #"^[^<]*>" "")
        (str/replace #"<[^>]*>" " ")
        (str/replace #"&#160;|&nbsp;" " ")
        (str/replace #"&#36;|&#x24;|(?i)&dollar;" "$")
        (str/replace #"&amp;" "&")
        str/trim)))

(defn- parse-ix-number
  "Number from an ix:nonFraction body, times 10^scale.

  A '$' decimal point left outside the tag is part of the number.
  ($.98 per share) is 0.98, not 98. ($1. followed by a tag of 25) is 1.25."
  [visible text scale]
  (let [raw (-> text str str/trim (str/replace "," ""))
        joined (cond
                 (not (re-matches #"-?[0-9]+(?:\.[0-9]+)?" raw)) nil
                 (re-matches #"[0-9]+" raw)
                 (cond
                   (re-find #"\$\s*(\d+)\.\s*$" visible)
                   (str (second (re-find #"\$\s*(\d+)\.\s*$" visible)) "." raw)
                   (re-find #"\$\s*\.\s*$" visible) (str "0." raw)
                   :else raw)
                 :else raw)
        v (when joined
            (try (Double/parseDouble joined)
                 (catch Exception _ nil)))]
    (when v
      (* v (Math/pow 10 (or scale 0))))))

(defn leading-currency-decimal
  "Corrected per-share amount when `concept-name` at period-end `end` is
  tagged with the decimal point outside the fact, as in ($.98 per share).
  nil when the filing does not split the number that way."
  [html concept-name end reported-val]
  (let [s (str html)
        contexts (xbrl-contexts s)
        m (re-matcher #"<ix:nonFraction\b([^>]*)>([^<]*)" s)]
    (loop []
      (when (.find m)
        (let [attrs (.group m 1)
              text (.group m 2)
              scale (try (parse-long (or (ix-attr attrs "scale") "0"))
                         (catch Exception _ nil))
              plain (when scale (parse-ix-number "" text scale))
              ctx-end (:end (get contexts (ix-attr attrs "contextRef")))]
          (if (and scale
                   (= concept-name (ix-attr attrs "name"))
                   (= (str end) (str ctx-end))
                   plain
                   (< (Math/abs (- (double plain) (double reported-val))) 1e-4))
            (let [corrected (parse-ix-number (visible-before s (.start m)) text scale)]
              (if (and corrected
                       (> (Math/abs (- corrected (double reported-val))) 1e-4))
                corrected
                (recur)))
            (recur)))))))

(def weighted-share-names
  {"us-gaap:WeightedAverageNumberOfSharesOutstandingBasic" "Shares Basic"
   "us-gaap:WeightedAverageNumberOfDilutedSharesOutstanding" "Shares Diluted"})

(defn- ix-share-facts
  "Weighted-average share facts from inline XBRL. Nested tags with no text
  are skipped; the inner tag carries the number. A '$' decimal point left
  outside the tag is part of the number."
  [html]
  (let [s (str html)
        m (re-matcher #"<ix:nonFraction\b([^>]*)>([^<]*)" s)]
    (loop [acc []]
      (if-not (.find m)
        acc
        (let [attrs (.group m 1)
              name (ix-attr attrs "name")]
          (if-not (contains? weighted-share-names name)
            (recur acc)
            (let [scale (try (parse-long (or (ix-attr attrs "scale") "0"))
                             (catch Exception _ nil))
                  val (when scale
                        (parse-ix-number (visible-before s (.start m))
                                         (.group m 2)
                                         scale))]
              (recur (if val
                       (conj acc {:name name
                                  :context (ix-attr attrs "contextRef")
                                  :val val})
                       acc)))))))))

(defn- annual-share-rows
  [facts contexts concept]
  (keep (fn [{:keys [name context val]}]
          (when (= concept name)
            (when-let [ctx (get contexts context)]
              (when (annual-duration? ctx)
                {:member (:member ctx) :end (:end ctx) :val val}))))
        facts))

(defn- listed-share-class
  "Share class to show. nil member means an unscoped total, which wins.
   Otherwise the class with the largest count (the listed class when a
   second class is a handful of shares)."
  [rows]
  (when (seq rows)
    (if (some #(nil? (:member %)) rows)
      {:member nil}
      {:member (->> rows
                    (group-by :member)
                    (apply max-key (fn [[_ xs]] (reduce max (map :val xs))))
                    key)})))

(defn- end-values
  [rows wanted]
  (into {}
        (keep (fn [{:keys [member end val]}]
                (when (= wanted member)
                  [end val]))
              rows)))

(defn- entity-common-shares
  "Point-in-time shares outstanding from company facts.

  Used when weighted-average share tags are missing (common for IFRS
  20-F filers such as GSK)."
  [cik form]
  (try
    (->> (ds/mapseq-reader
          (e/facts cik :concept "EntityCommonStockSharesOutstanding" :form form))
         (filter #(and (= "dei" (str (:taxonomy %)))
                       (or (= "shares" (str (:unit %)))
                           (str/ends-with? (str (:unit %)) "/shares"))
                       (number? (:val %))
                       (pos? (double (:val %)))))
         latest-filed
         (map (juxt #(str (:end %)) :val))
         (into {}))
    (catch Throwable _
      {})))

(defn class-weighted-shares
  "Shares Basic and Shares Diluted from the latest annual report.

  Company facts drop dimensional share counts, so a dual-class filer has
  no share row. Read the weighted-average facts from the filing and keep
  the listed class. When those tags are absent, fall back to
  dei:EntityCommonStockSharesOutstanding from company facts."
  [cik form]
  (try
    (let [filing (e/filing cik :form form)
          html (when filing (e/filing-document filing (:primaryDocument filing)))
          contexts (xbrl-contexts html)
          facts (ix-share-facts html)
          basic-name "us-gaap:WeightedAverageNumberOfSharesOutstandingBasic"
          diluted-name "us-gaap:WeightedAverageNumberOfDilutedSharesOutstanding"
          basic-rows (annual-share-rows facts contexts basic-name)
          diluted-rows (annual-share-rows facts contexts diluted-name)
          member (:member (or (listed-share-class basic-rows)
                              (listed-share-class diluted-rows)))
          weighted (into {}
                         (keep (fn [[concept line]]
                                 (let [values (end-values (if (= concept basic-name)
                                                            basic-rows
                                                            diluted-rows)
                                                          member)]
                                   (when (seq values) [line values]))))
                         weighted-share-names)]
      (if (seq weighted)
        weighted
        (let [outstanding (entity-common-shares cik form)]
          (if (seq outstanding)
            {"Shares Basic" outstanding}
            {}))))
    (catch Throwable _
      {})))

(defn- shares-present?
  [by-pl]
  (boolean (some (fn [line]
                   (some number? (vals (get by-pl line))))
                 ["Shares Basic" "Shares Diluted"])))

(defn- with-class-shares
  [by-pl cik form]
  (if (shares-present? by-pl)
    by-pl
    (merge by-pl (class-weighted-shares cik form))))

(defn financials-data
  "Standardized P&L and main balance-sheet lines.

  `:n` is how many columns to keep: fiscal years when annual, quarters
  when quarterly. `:period` is annual (default) or quarterly.

  Returns `{:ticker :name :cik :form :period :columns :rows}`."
  [m]
  (let [{:keys [ticker n period]} (opts m)]
    (ensure-identity!)
    (let [cik (resolve-filer ticker)
          meta (e/company-metadata cik)
          form (annual-form cik)
          fye (:fiscal-year-end meta)
          quarterly? (= period :quarterly)
          source (if quarterly?
                   (quarterly-statements cik fye form)
                   (let [annual (annual-statement-rows cik fye form)
                         bs (annual-balance-rows cik fye form)
                         cash (annual-cashflow-rows cik fye form cashflow-extra-concepts)
                         items (vec (distinct (map :line-item annual)))]
                     {:income-values (into {} (for [item items]
                                                [item (annual-line-values annual item)]))
                      :balance-values (into {} (for [item (distinct (map :line-item bs))]
                                                 [item (annual-line-values bs item)]))
                      :dividends (annual-line-values cash dividends-paid-label)
                      :repurchases (annual-line-values cash stock-repurchase-label)}))
          by-pl* (cond-> (:income-values source)
                   (not quarterly?) (with-class-shares cik form))
          by-bs (:balance-values source)
          ends (period-ends (get by-pl* "Revenue")
                            (get by-bs "Total Assets")
                            n)
          labels (column-labels ends period (fye-month fye))
          ;; Quarterly columns include Q4 from the annual report.
          filing-forms (if quarterly? ["10-Q" form] [form])
          filing-dates (filing-dates-by-end cik filing-forms)
          table (statement-rows by-pl* by-bs (:dividends source) (:repurchases source)
                                ends labels filing-dates)]
      (merge {:ticker ticker
              :name (:name meta)
              :cik cik
              :form (if quarterly? "10-Q" form)
              :period period}
             table))))

(defn financials
  "Print standardized P&L and main balance-sheet lines.

  `:n` is fiscal years when annual (the default), or quarters when
  `:period` is quarterly.

  Usage: clj -X:financials :ticker IMPP :n 20
         clj -X:financials :ticker AAPL :period quarterly :n 10"
  ([] (financials {}))
  ([m]
   (let [{:keys [ticker cik form columns rows] :as data} (financials-data m)
         ds (ds/->dataset rows
                          {:dataset-name (str ticker " financials ($ millions; EPS in $)")
                           :column-order columns})]
     (println)
     (println (format "%s  %s  CIK=%s  %s  ($ millions; EPS in $)"
                      ticker (:name data) cik form))
     (pprint/print-table columns rows)
     ds)))

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
           annual (filter annual-duration? reported)
           latest-end (->> annual (map #(str (:end %))) sort last)
           line-by-concept (->> (annual-statement-rows cik fye)
                                (filter #(= latest-end (str (:end %))))
                                (map (juxt :concept :line-item))
                                (into {}))
           fields (->> annual
                       (filter #(= latest-end (str (:end %))))
                       (group-by :concept)
                       vals
                       (map latest-by-filed)
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

(defn -main
  [& _]
  (download)
  (report))
