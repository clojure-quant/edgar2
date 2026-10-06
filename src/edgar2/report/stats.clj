(ns edgar2.report.stats
  "Universe fundamentals from companyfacts.zip: sales growth, margin, ROC."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.download.facts :as dl]
            [edgar2.download.price :as price]
            [edgar2.download.custom.universe :as universe]
            [jsonista.core :as json])
  (:import [java.time LocalDate]
           [java.time.temporal ChronoUnit]
           [java.util.zip ZipFile]))

(def stats-path "data/stats.edn")

(def net-income-tags
  "US GAAP Net Income (Loss), then IFRS profit as Net Income."
  [[:us-gaap :NetIncomeLoss]
   [:ifrs-full :ProfitLoss]
   [:ifrs-full :ProfitLossAttributableToOwnersOfParent]])

(def eps-tags
  [[:us-gaap :EarningsPerShareDiluted]
   [:us-gaap :EarningsPerShareBasic]
   [:ifrs-full :DilutedEarningsLossPerShare]
   [:ifrs-full :BasicEarningsLossPerShare]
   [:ifrs-full :BasicAndDilutedEarningsLossPerShare]])

(def dps-tags
  [[:us-gaap :CommonStockDividendsPerShareDeclared]
   [:us-gaap :CommonStockDividendsPerShareCashPaid]
   [:us-gaap :CommonStockDividendsPerShare]
   [:ifrs-full :DividendsPaidOrdinaryShares]])

(def asset-tags
  [[:us-gaap :Assets]
   [:ifrs-full :Assets]])

(def ebit-tags
  "Operating income. Fallback is net income + interest + tax."
  [[:us-gaap :OperatingIncomeLoss]
   [:ifrs-full :ProfitLossFromOperatingActivities]])

(def interest-expense-tags
  [[:us-gaap :InterestExpense]
   [:ifrs-full :InterestExpense]
   [:ifrs-full :InterestExpenseOnBorrowings]])

(def income-tax-tags
  [[:us-gaap :IncomeTaxExpenseBenefit]
   [:ifrs-full :IncomeTaxExpenseContinuingOperations]])

(def cash-tag-pairs
  [[:us-gaap :CashAndCashEquivalentsAtCarryingValue]
   [:ifrs-full :CashAndCashEquivalents]
   [:us-gaap :Cash]])

(def annual-forms
  #{"10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"})

(def sales-growth-window
  "Number of trailing YoY revenue changes averaged into :sales-growth-yoy."
  7)

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
  [obs]
  (when-let [days (period-days (:start obs) (:end obs))]
    (<= 300 days 400)))

(defn observations
  [facts tag-pairs]
  (mapcat (fn [[tax tag]] (universe/concept-observations facts tax tag))
          tag-pairs))

(defn prefer-unit
  [obs unit]
  (if (str/blank? (str unit))
    obs
    (let [matched (filter #(= unit (:unit %)) obs)]
      (if (seq matched) matched obs))))

(defn latest-by-end
  [obs]
  (->> obs
       (filter #(and (number? (:val %)) (not (str/blank? (str (:end %))))))
       (group-by #(str (:end %)))
       vals
       (map (fn [rs] (last (sort-by #(str (:filed %)) rs))))
       (sort-by #(str (:end %)))
       vec))

(defn annual-flows
  "Duration facts covering ~12 months, one value per period-end."
  [facts tag-pairs unit]
  (latest-by-end
   (filter annual-duration?
           (prefer-unit (observations facts tag-pairs) unit))))

(defn first-annual-flows
  "Annual series from the first tag pair that has data (do not mix tags)."
  [facts tag-pairs unit]
  (or (some (fn [pair]
              (let [s (annual-flows facts [pair] unit)]
                (when (seq s) s)))
            tag-pairs)
      []))

(defn annual-stocks
  "Instant facts from annual filings (FY / 10-K / 20-F / 40-F)."
  [facts tag-pairs unit]
  (latest-by-end
   (filter #(or (= "FY" (:fp %))
                (annual-forms (str (:form %))))
           (prefer-unit (observations facts tag-pairs) unit))))

(defn yoy-pct
  [prev cur]
  (when (and (number? prev) (number? cur) (not (zero? (double prev))))
    (* 100.0 (/ (- (double cur) (double prev)) (double prev)))))

(defn avg
  [xs]
  (when (seq xs)
    (/ (double (reduce + xs)) (count xs))))

(defn avg-sales-growth
  "Mean of the last `n` year-over-year revenue changes (%)."
  [revenue-annual n]
  (let [vals (mapv :val revenue-annual)
        growths (vec (keep identity (map yoy-pct vals (rest vals))))
        window (take-last n growths)]
    (when (seq window)
      {:sales-growth-yoy (avg window)
       :sales-growth-years (count window)})))

(defn by-end
  [series]
  (into {} (map (juxt #(str (:end %)) :val) series)))

(defn latest-aligned
  "Latest period-end that exists in both series."
  [num-series den-series]
  (let [n (by-end num-series)
        d (by-end den-series)
        ends (sort (set/intersection (set (keys n)) (set (keys d))))]
    (when (seq ends)
      (let [end (last ends)]
        {:end end :num (n end) :den (d end)}))))

(defn ratio
  [num den]
  (when (and (number? num) (number? den) (not (zero? (double den))))
    (/ (double num) (double den))))

(defn usd-unit?
  "True for USD or USD/shares (US listing price is in dollars)."
  [u]
  (let [s (str/upper-case (str u))]
    (or (= s "USD")
        (str/starts-with? s "USD/"))))

(defn latest-obs
  [series]
  (when (seq series)
    (last series)))

(defn value-at
  [series end]
  (when (and (seq series) end)
    (get (by-end series) (str end))))

(defn approx=
  "True when a and b are within 2% or 1 unit (tag double-counts)."
  [a b]
  (and (number? a) (number? b)
       (let [a (double a)
             b (double b)]
         (<= (Math/abs (- a b))
             (max 1.0 (* 0.02 (Math/abs b)))))))

(defn sum-unique
  "Sum xs, counting near-duplicate tags once."
  [xs]
  (loop [acc [] xs (filter number? xs)]
    (if-let [x (first xs)]
      (if (some #(approx= % x) acc)
        (recur acc (next xs))
        (recur (conj acc (double x)) (next xs)))
      (when (seq acc)
        (reduce + acc)))))

(defn best-annual-flows
  "Annual series from the tag whose latest period-end is most recent."
  [facts tag-pairs unit]
  (let [series (keep (fn [pair]
                        (let [s (annual-flows facts [pair] unit)]
                          (when (seq s) s)))
                      tag-pairs)]
    (when (seq series)
      (apply max-key #(str (:end (last %))) series))))

(defn stock-at
  [facts tag-pairs unit end]
  (value-at (annual-stocks facts tag-pairs unit) end))

(defn balance-sheet-end
  [assets]
  (some-> (latest-obs assets) :end str))

(defn interest-bearing-debt
  "Interest-bearing debt at annual balance-sheet `end`.

  Term debt is the noncurrent portion plus the current portion when both
  are tagged, otherwise the long-term debt total (US-GAAP LongTermDebt or
  IFRS Borrowings). Commercial paper and other short-term borrowings are
  added unless they repeat that current portion."
  [facts unit end]
  (when end
    (let [at (fn [pairs] (stock-at facts pairs unit end))
          non (or (at [[:us-gaap :LongTermDebtNoncurrent]])
                  (at [[:ifrs-full :LongtermBorrowings]]))
          cur (or (at [[:us-gaap :LongTermDebtCurrent]])
                  (at [[:ifrs-full :CurrentBorrowingsAndCurrentPortionOfNoncurrentBorrowings]])
                  (at [[:ifrs-full :ShorttermBorrowings]]))
          ltd (or (at [[:us-gaap :LongTermDebt]])
                  (at [[:ifrs-full :Borrowings]]))
          term (cond
                 (and (number? non) (number? cur)) (+ (double non) (double cur))
                 (and (number? non) (number? ltd)
                      (> (double ltd) (* 1.01 (double non)))) (double ltd)
                 (number? non) (double non)
                 (number? ltd) (double ltd)
                 (number? cur) (double cur))
          already (when (and (number? cur) (number? term)
                             (or (and (number? non) (number? cur))
                                 (approx= ltd term)))
                    cur)
          extras (sum-unique
                  (remove #(approx= % already)
                          [(at [[:us-gaap :CommercialPaper]])
                           (at [[:us-gaap :ShortTermBorrowings]])
                           (at [[:us-gaap :OtherShortTermBorrowings]])]))]
      (when (or term extras)
        (+ (double (or term 0)) (double (or extras 0)))))))

(defn cash-at
  [facts unit end]
  (when end
    (some #(stock-at facts [%] unit end) cash-tag-pairs)))

(defn annual-ebit
  "Latest annual operating income, else net income + interest + tax
  at the latest net-income period-end."
  [facts unit]
  (let [reported (latest-obs (best-annual-flows facts ebit-tags unit))]
    (if (and reported (number? (:val reported)))
      {:ebit (:val reported) :ebit-end (str (:end reported))}
      (let [ni (latest-obs (first-annual-flows facts net-income-tags unit))
            end (some-> ni :end str)
            interest (value-at (best-annual-flows facts interest-expense-tags unit) end)
            tax (value-at (best-annual-flows facts income-tax-tags unit) end)]
        (when (and ni (number? (:val ni)) end
                   (or (number? interest) (number? tax)))
          {:ebit (+ (double (:val ni))
                    (double (or interest 0))
                    (double (or tax 0)))
           :ebit-end end})))))

(defn annual-eps
  "Latest annual diluted/basic EPS series (do not force the revenue unit)."
  [facts]
  (let [flows (first-annual-flows facts eps-tags nil)]
    (if (seq flows)
      flows
      (annual-stocks facts eps-tags nil))))

(defn annual-dps
  "Latest annual common dividend per share."
  [facts]
  (let [flows (first-annual-flows facts dps-tags nil)]
    (if (seq flows)
      flows
      (annual-stocks facts dps-tags nil))))

(defn company-stats
  [facts {:keys [revenue-unit] :as row}]
  (let [rev (annual-flows facts universe/revenue-tags revenue-unit)
        ni (first-annual-flows facts net-income-tags revenue-unit)
        assets (annual-stocks facts asset-tags revenue-unit)
        eps (latest-obs (annual-eps facts))
        dps (latest-obs (annual-dps facts))
        growth (avg-sales-growth rev sales-growth-window)
        margin (latest-aligned ni rev)
        roc (latest-aligned ni assets)
        bs-end (balance-sheet-end assets)
        debt (interest-bearing-debt facts revenue-unit bs-end)
        cash (cash-at facts revenue-unit bs-end)
        preferred (stock-at facts [[:us-gaap :PreferredStockValue]] revenue-unit bs-end)
        nci (or (stock-at facts [[:us-gaap :MinorityInterest]] revenue-unit bs-end)
                (stock-at facts [[:ifrs-full :NoncontrollingInterests]] revenue-unit bs-end))
        ebit (annual-ebit facts revenue-unit)]
    (cond-> (select-keys row [:ticker :exchange :cik :entityName :revenue-unit])
      (:sales-growth-yoy growth)
      (assoc :sales-growth-yoy (:sales-growth-yoy growth)
             :sales-growth-years (:sales-growth-years growth))
      margin
      (assoc :profit-margin (ratio (:num margin) (:den margin))
             :net-profit (:num margin)
             :revenue (:den margin)
             :margin-end (:end margin))
      roc
      (assoc :return-on-capital (ratio (:num roc) (:den roc))
             :assets (:den roc)
             :roc-end (:end roc))
      (and eps (number? (:val eps)))
      (assoc :eps (:val eps)
             :eps-unit (:unit eps)
             :eps-end (:end eps))
      (and dps (number? (:val dps)))
      (assoc :dps (:val dps)
             :dps-unit (:unit dps)
             :dps-end (:end dps))
      (number? debt) (assoc :debt debt :debt-end bs-end)
      (number? cash) (assoc :cash cash)
      (number? preferred) (assoc :preferred-stock preferred)
      (number? nci) (assoc :noncontrolling-interest nci)
      (:ebit ebit) (assoc :ebit (:ebit ebit) :ebit-end (:ebit-end ebit)))))

(defn load-price-by-ticker
  "ticker → close from data/prices.edn."
  []
  (into {}
        (map (fn [{:keys [ticker close]}]
               [(str/upper-case (str ticker)) (double close)])
             (price/load-prices))))

(defn with-valuation
  "Add :shares, :price, :marketcap, :price-sales, :price-earnings,
  :enterprise-value, and :ev-ebit.
  PE is price / USD EPS, else USD market cap / USD Net Income — never mix
  a dollar price with ARS/JPY/etc. earnings.
  Enterprise value is USD market cap + debt + preferred + noncontrolling
  interest − cash. EV/EBIT divides that by USD EBIT."
  [st price-by-ticker shares]
  (let [price (get price-by-ticker (str/upper-case (str (:ticker st))))
        mcap (when (and price (number? shares))
               (* (double price) (double shares)))
        rev (when (number? (:revenue st)) (:revenue st))
        ni (when (number? (:net-profit st)) (:net-profit st))
        eps (when (number? (:eps st)) (:eps st))
        usd-eps? (usd-unit? (:eps-unit st))
        usd-pl? (usd-unit? (:revenue-unit st))
        dps (when (number? (:dps st)) (:dps st))
        usd-dps? (usd-unit? (:dps-unit st))
        pe (cond
             (and price (number? eps) usd-eps? (not (zero? (double eps))))
             (/ (double price) (double eps))
             (and mcap usd-pl? (number? ni) (not (zero? (double ni))))
             (/ mcap (double ni)))
        dy (when (and price (pos? (double price)) (number? dps) usd-dps?)
             (* 100.0 (/ (double dps) (double price))))
        ebit (when (number? (:ebit st)) (:ebit st))
        ev (when (and mcap usd-pl?)
             (+ (double mcap)
                (double (or (:debt st) 0))
                (double (or (:preferred-stock st) 0))
                (double (or (:noncontrolling-interest st) 0))
                (- (double (or (:cash st) 0)))))]
    (cond-> st
      (number? shares) (assoc :shares shares)
      price (assoc :price price)
      mcap (assoc :marketcap mcap)
      (and mcap usd-pl? rev (not (zero? (double rev))))
      (assoc :price-sales (/ mcap (double rev)))
      pe (assoc :price-earnings pe)
      dy (assoc :dividend-yield dy)
      ev (assoc :enterprise-value ev)
      (and ev (number? ebit) (not (zero? (double ebit))))
      (assoc :ev-ebit (/ ev (double ebit))))))

(defn load-universe
  []
  (when-not (.exists (io/file universe/universe-path))
    (throw (ex-info "Missing universe file; run clj -X:universe first"
                    {:path universe/universe-path})))
  (edn/read-string (slurp universe/universe-path)))

(defn read-cik-json
  [zf cik]
  (when cik
    (let [entry (.getEntry zf (str "CIK" cik ".json"))]
      (when entry
        (with-open [in (.getInputStream zf entry)]
          (json/read-value in json/keyword-keys-object-mapper))))))

(def stats-field-order
  [:ticker :exchange :cik :entityName :price
   :shares :marketcap
   :revenue :revenue-unit
   :net-profit
   :eps
   :assets
   :sales-growth-yoy
   :return-on-capital
   :price-earnings
   :dividend-yield
   :enterprise-value
   :ev-ebit
   :ebit
   :debt
   :cash])

(defn select-fields
  "Map with `ks` first (present keys only); remaining keys last, sorted."
  [m ks]
  (let [seen (set ks)
        front (mapcat (fn [k] (when (contains? m k) [k (get m k)])) ks)
        back (->> (keys m)
                  (remove seen)
                  sort
                  (mapcat (fn [k] [k (get m k)])))]
    (apply array-map (concat front back))))

(defn save-stats!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str stats-path ".tmp")
        ordered (mapv #(select-fields % stats-field-order) rows)]
    (spit tmp (with-out-str (pprint/pprint ordered)))
    (.renameTo (io/file tmp) (io/file stats-path))))

(defn fmt-growth
  [x]
  (when x (format "%.1f" (double x))))

(defn round1
  [x]
  (when x (/ (Math/round (* (double x) 10.0)) 10.0)))

(defn round4
  [x]
  (when x (/ (Math/round (* (double x) 10000.0)) 10000.0)))

(defn stats
  "From data/universe.edn + companyfacts.zip + prices.edn, write data/stats.edn
  with 7-year mean YoY sales growth (%), profit margin, return on capital
  (Net Income / assets), plus :shares, :price, :marketcap, :price-sales,
  :price-earnings (USD price / USD EPS, else USD market cap / USD Net Income),
  :enterprise-value (USD market cap + debt + preferred + NCI − cash),
  and :ev-ebit.

  Usage: clj -X:stats
         clj -X:stats :limit 25"
  ([] (stats {}))
  ([{:keys [limit force]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         all (load-universe)
         prices (load-price-by-ticker)
         filers (vec (cond->> all limit (take (long limit))))
         total (count filers)]
     (println (format "stats  %d of %d universe  ← %s"
                      total (count all) zip-path))
     (flush)
     (with-open [zf (ZipFile. (str zip-path))]
       (loop [i 0
              acc (transient [])
              rows filers]
         (if-let [row (first rows)]
           (let [data (try (read-cik-json zf (:cik row))
                           (catch Exception ex
                             (println (format "  skip %s (%s)" (:cik row) (.getMessage ex)))
                             nil))
                 st (try
                      (-> (if data
                            (company-stats (:facts data) row)
                            (select-keys row [:ticker :exchange :cik :entityName]))
                          (with-valuation prices (:shares-outstanding row)))
                      (catch Exception ex
                        (println (format "  stats-fail %s (%s)" (:ticker row) (.getMessage ex)))
                        (select-keys row [:ticker :exchange :cik :entityName])))
                 i' (inc i)]
             (when (or (zero? (mod i' 250)) (= i' total))
               (println (format "  %d/%d  %s  growth=%s  margin=%s  roc=%s"
                                i' total
                                (or (:ticker st) (:cik st))
                                (fmt-growth (:sales-growth-yoy st))
                                (some-> (:profit-margin st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-capital st) (* 100) (#(format "%.1f" %)))))
               (flush))
             (recur i' (conj! acc st) (next rows)))
           (let [out (vec (persistent! acc))]
             (save-stats! out)
             (println)
             (pprint/print-table
              [:ticker :price :marketcap :price-sales :price-earnings
               :sales-growth-yoy :profit-margin :return-on-capital]
              (take 25
                    (map (fn [r]
                           (-> r
                               (update :sales-growth-yoy round1)
                               (update :profit-margin round4)
                               (update :return-on-capital round4)
                               (update :price-sales round4)
                               (update :price-earnings round4)))
                         out)))
             (println (format "Wrote %s  (%d companies; growth=%d  margin=%d  roc=%d  price=%d  mcap=%d  pe=%d  ev=%d  ev-ebit=%d)"
                              stats-path
                              (count out)
                              (count (filter :sales-growth-yoy out))
                              (count (filter :profit-margin out))
                              (count (filter :return-on-capital out))
                              (count (filter :price out))
                              (count (filter :marketcap out))
                              (count (filter :price-earnings out))
                              (count (filter :enterprise-value out))
                              (count (filter :ev-ebit out))))
             out)))))))
