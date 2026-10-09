(ns edgar2.report.stats
  "Universe fundamentals from companyfacts.zip: sales growth, margin, ROC."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.derived :as derived]
            [edgar2.download.custom.fsds :as fsds]
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

(def dps-cash-tags
  "Per-share cash dividend. Used when it falls on the last report date."
  [[:us-gaap :CommonStockDividendsPerShareCashPaid]])

(def dps-other-tags
  "Other per-share dividends, used only when they fall on the last report date."
  [[:us-gaap :CommonStockDividendsPerShare]
   [:us-gaap :CommonStockDividendsPerShareDeclared]
   [:ifrs-full :DividendsPaidOrdinaryShares]])

(def dividends-paid-tags
  "Total cash dividends. US-GAAP total, then common-only, then IFRS financing."
  [[:us-gaap :PaymentsOfDividends]
   [:us-gaap :PaymentsOfDividendsCommonStock]
   [:ifrs-full :DividendsPaidClassifiedAsFinancingActivities]
   [:ifrs-full :DividendsPaidToEquityHoldersOfParentClassifiedAsFinancingActivities]
   [:ifrs-full :DividendsPaid]])

(def asset-tags
  [[:us-gaap :Assets]
   [:ifrs-full :Assets]])

(def total-equity-tags
  "Total equity, already including noncontrolling interest."
  [[:us-gaap :StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest]
   [:ifrs-full :Equity]])

(def parent-equity-tags
  "Equity attributable to the parent. This is total equity when
  noncontrolling interest is not tagged."
  [[:us-gaap :StockholdersEquity]
   [:ifrs-full :EquityAttributableToOwnersOfParent]])

(def nci-tags
  [[:us-gaap :MinorityInterest]
   [:ifrs-full :NoncontrollingInterests]])

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

(def profit-margin-window
  "Number of trailing annual profit margins averaged into :avg-profit-margin."
  10)

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

(defn first-annual-stocks
  "Annual instant series from the first tag pair that has data (do not mix tags)."
  [facts tag-pairs unit]
  (or (some (fn [pair]
              (let [s (annual-stocks facts [pair] unit)]
                (when (seq s) s)))
            tag-pairs)
      []))

(defn yoy-pct
  [prev cur]
  (when (and (number? prev) (number? cur) (not (zero? (double prev))))
    (* 100.0 (/ (- (double cur) (double prev)) (double prev)))))

(defn avg
  [xs]
  (when (seq xs)
    (/ (double (reduce + xs)) (count xs))))

(defn direction-score
  "Share of years that did not fall versus the prior year, from 0 to 100.

  Each annual value after the first scores 1 when value − prior >= 0,
  otherwise 0. The score is 100 times that count divided by the number of
  year-to-year steps. The first year has no prior year, so it is omitted."
  [series]
  (let [vals (mapv :val series)
        steps (keep (fn [[prev cur]]
                      (when (and (number? prev) (number? cur))
                        (if (>= (double cur) (double prev)) 1 0)))
                    (map vector vals (rest vals)))]
    (when (seq steps)
      (* 100.0 (/ (double (reduce + steps)) (count steps))))))

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

(defn plus-at-end
  "Add `extra` onto `base` where both share a period-end."
  [base extra]
  (if (empty? extra)
    (vec base)
    (let [extra-by (by-end extra)]
      (mapv (fn [row]
              (if-let [v (get extra-by (str (:end row)))]
                (update row :val #(+ (double %) (double v)))
                row))
            base))))

(defn total-equity
  "Total equity. Prefer a tag that already includes noncontrolling interest.
  Otherwise parent equity, plus noncontrolling interest on years that tag it."
  [facts unit]
  (let [inclusive (first-annual-stocks facts total-equity-tags unit)]
    (if (seq inclusive)
      inclusive
      (plus-at-end (first-annual-stocks facts parent-equity-tags unit)
                   (first-annual-stocks facts nci-tags unit)))))

(defn aligned-ends
  "Period-ends present in both series, oldest first."
  [num-series den-series]
  (sort (set/intersection (set (keys (by-end num-series)))
                          (set (keys (by-end den-series))))))

(defn latest-aligned
  "Latest period-end that exists in both series."
  [num-series den-series]
  (let [n (by-end num-series)
        d (by-end den-series)
        ends (aligned-ends num-series den-series)]
    (when (seq ends)
      (let [end (last ends)]
        {:end end :num (n end) :den (d end)}))))

(defn ratio
  [num den]
  (when (and (number? num) (number? den) (not (zero? (double den))))
    (/ (double num) (double den))))

(defn yearly-ratios
  "num/den for each shared period-end, oldest first.
  A year with a zero denominator is omitted."
  [num-series den-series]
  (let [n (by-end num-series)
        d (by-end den-series)]
    (vec (keep (fn [end] (ratio (n end) (d end)))
               (aligned-ends num-series den-series)))))

(defn avg-profit-margin
  "Mean of the last `n` annual profit margins (net income / revenue)."
  [ni rev n]
  (let [window (take-last n (yearly-ratios ni rev))]
    (when (seq window)
      {:avg-profit-margin (avg window)
       :avg-profit-margin-years (count window)})))

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

(defn later-end
  "Later of two period-end strings."
  [a b]
  (->> [a b] (remove str/blank?) sort last))

(defn annual-revenue
  "Annual revenue for sales growth and the revenue score.

  One IFRS series, not a mix: `ifrs-full` Revenue, or
  RevenueFromSaleOfGoods when that series' latest period-end is later.
  When both end on the same date, Revenue is the total and wins.
  With neither tag, US-GAAP revenue tags are used."
  [facts unit]
  (let [total (annual-flows facts [[:ifrs-full :Revenue]] unit)
        goods (annual-flows facts [[:ifrs-full :RevenueFromSaleOfGoods]] unit)
        total-end (some-> (latest-obs total) :end str)
        goods-end (some-> (latest-obs goods) :end str)
        end (later-end total-end goods-end)
        chosen (cond
                 (= end total-end) total
                 (= end goods-end) goods)]
    (if (seq chosen)
      chosen
      (annual-flows facts universe/revenue-tags unit))))

(defn fact-at
  "First annual fact among `tag-pairs` whose period-end is `end`."
  [facts tag-pairs unit end]
  (when-not (str/blank? (str end))
    (some (fn [pair]
            (some (fn [series]
                    (when-let [v (value-at series end)]
                      (when (number? v)
                        {:val v
                         :end (str end)
                         :unit (:unit (some #(when (= (str end) (str (:end %))) %) series))})))
                  [(annual-flows facts [pair] unit)
                   (annual-stocks facts [pair] unit)]))
          tag-pairs)))

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

(defn first-present
  "Period-end → value. Earlier maps win when an end is in more than one."
  [maps]
  (reduce (fn [acc m] (merge m acc)) {} maps))

(defn statement-by-item
  "Annual {line {end value}} for the lines edgar2.derived fills.
  Per period, the first concept in the line's chain that has an annual fact."
  [facts unit]
  (into {}
        (keep (fn [[label tags]]
                (let [by-end (first-present
                              (keep (fn [pair]
                                      (let [series (annual-flows facts [pair] unit)]
                                        (when (seq series)
                                          (into {} (map (juxt #(str (:end %)) :val) series)))))
                                    tags))]
                  (when (seq by-end)
                    [label by-end])))
              derived/statement-lines)))

(defn with-derived-income
  "Reported lines plus derived operating expenses and, where those
  expenses were derived and operating income is absent, operating income
  = gross profit − operating expenses.

  `:derived-oi` is only the operating-income periods this call filled."
  [facts unit]
  (let [by-item (statement-by-item facts unit)
        ends (->> (vals by-item) (mapcat keys) distinct sort vec)
        filled (derived/with-derived-lines by-item ends)
        before (get by-item derived/operating-income)]
    {:lines filled
     :derived-oi (into {}
                       (keep (fn [[end v]]
                               (when (and (number? v)
                                          (not (number? (get before end))))
                                 [end v]))
                             (get filled derived/operating-income)))}))

(defn series-plus
  "Reported rows, then values from `extra` on period-ends the series lacks."
  [reported extra]
  (let [have (set (map #(str (:end %)) reported))]
    (->> (concat reported
                 (keep (fn [[end v]]
                         (when (and (number? v) (not (have (str end))))
                           {:end (str end) :val (double v)}))
                       extra))
         (sort-by #(str (:end %)))
         vec)))

(defn annual-ebit
  "Operating income at the last report `end`. Otherwise derived operating
  income at that date (gross profit − derived operating expenses).
  Otherwise net income + interest + tax at that same date."
  [facts unit end derived-oi]
  (or (when-let [reported (fact-at facts ebit-tags unit end)]
        {:ebit (:val reported) :ebit-end (:end reported)})
      (when-let [v (get derived-oi (str end))]
        (when (number? v)
          {:ebit (double v) :ebit-end (str end)}))
      (let [ni (fact-at facts net-income-tags unit end)
            interest (fact-at facts interest-expense-tags unit end)
            tax (fact-at facts income-tax-tags unit end)]
        (when (and ni (or interest tax))
          {:ebit (+ (double (:val ni))
                    (double (or (:val interest) 0))
                    (double (or (:val tax) 0)))
           :ebit-end (str end)}))))

(defn flow-or-stock
  [facts pair unit]
  (let [flows (annual-flows facts [pair] unit)]
    (if (seq flows)
      flows
      (annual-stocks facts [pair] unit))))

(defn basic-if-diluted-older
  "Basic series when its latest period-end is after diluted's."
  [diluted basic]
  (let [de (some-> (latest-obs diluted) :end str)
        be (some-> (latest-obs basic) :end str)]
    (cond
      (and de be (neg? (compare de be))) basic
      (seq diluted) diluted
      (seq basic) basic)))

(defn annual-eps
  "Latest annual EPS. Basic wins when diluted's period-end is older."
  [facts]
  (or (basic-if-diluted-older
       (flow-or-stock facts [:us-gaap :EarningsPerShareDiluted] nil)
       (flow-or-stock facts [:us-gaap :EarningsPerShareBasic] nil))
      (basic-if-diluted-older
       (flow-or-stock facts [:ifrs-full :DilutedEarningsLossPerShare] nil)
       (flow-or-stock facts [:ifrs-full :BasicEarningsLossPerShare] nil))
      (seq (flow-or-stock facts [:ifrs-full :BasicAndDilutedEarningsLossPerShare] nil))
      []))

(defn annual-dps
  "Per-share dividend at the last report `end`. Cash paid wins. No fact
  on that date is 0."
  [facts end revenue-unit]
  (or (fact-at facts dps-cash-tags nil end)
      (fact-at facts dps-other-tags nil end)
      (when end
        {:val 0.0
         :end (str end)
         :unit (when (usd-unit? revenue-unit) "USD/shares")})))

(defn annual-dividends-paid
  "Cash dividends paid at period-end `end`, as a positive amount in `unit`.
  Nil when that fact is missing, zero, or in another currency.
  A negative cash-flow sign is treated as the amount paid."
  [facts unit end]
  (when-let [paid (fact-at facts dividends-paid-tags unit end)]
    (when (and (number? (:val paid))
               (or (str/blank? (str unit))
                   (= (str unit) (str (:unit paid)))))
      (let [amount (Math/abs (double (:val paid)))]
        (when (pos? amount) amount)))))

(defn dividend-coverage
  "Net income / dividends paid at `end`. Nil when no dividend was paid."
  [facts unit end net-income]
  (when-let [paid (annual-dividends-paid facts unit end)]
    (ratio net-income paid)))

(defn company-stats
  [facts {:keys [revenue-unit] :as row}]
  (let [rev (annual-flows facts universe/revenue-tags revenue-unit)
        ni (first-annual-flows facts net-income-tags revenue-unit)
        assets (annual-stocks facts asset-tags revenue-unit)
        bs-end (balance-sheet-end assets)
        last-end (later-end (some-> (latest-obs rev) :end str) bs-end)
        eps (latest-obs (annual-eps facts))
        dps (annual-dps facts last-end revenue-unit)
        income (with-derived-income facts revenue-unit)
        oi (series-plus (annual-flows facts ebit-tags revenue-unit)
                        (:derived-oi income))
        revenue (annual-revenue facts revenue-unit)
        growth (avg-sales-growth revenue sales-growth-window)
        margins (avg-profit-margin ni rev profit-margin-window)
        rev-score (direction-score revenue)
        oi-score (direction-score oi)
        equity (total-equity facts revenue-unit)
        margin (latest-aligned ni rev)
        roa (latest-aligned ni assets)
        roc (latest-aligned ni equity)
        debt (interest-bearing-debt facts revenue-unit bs-end)
        cash (cash-at facts revenue-unit bs-end)
        preferred (stock-at facts [[:us-gaap :PreferredStockValue]] revenue-unit bs-end)
        nci (or (stock-at facts [[:us-gaap :MinorityInterest]] revenue-unit bs-end)
                (stock-at facts [[:ifrs-full :NoncontrollingInterests]] revenue-unit bs-end))
        ebit (annual-ebit facts revenue-unit last-end
                             (:derived-oi income))
        coverage-end (or (:end margin) last-end)
        coverage-ni (if margin
                      (:num margin)
                      (:val (fact-at facts net-income-tags revenue-unit coverage-end)))
        coverage (dividend-coverage facts revenue-unit coverage-end coverage-ni)]
    (cond-> (select-keys row [:ticker :exchange :cik :entityName :revenue-unit])
      (:sales-growth-yoy growth)
      (assoc :sales-growth-yoy (:sales-growth-yoy growth)
             :sales-growth-years (:sales-growth-years growth))
      (number? rev-score) (assoc :revenue-score rev-score)
      (number? oi-score) (assoc :operating-income-score oi-score)
      margin
      (assoc :profit-margin (ratio (:num margin) (:den margin))
             :net-profit (:num margin)
             :revenue (:den margin)
             :margin-end (:end margin))
      (:avg-profit-margin margins)
      (assoc :avg-profit-margin (:avg-profit-margin margins)
             :avg-profit-margin-years (:avg-profit-margin-years margins))
      roa
      (assoc :return-on-assets (ratio (:num roa) (:den roa))
             :assets (:den roa)
             :roa-end (:end roa))
      roc
      (assoc :return-on-capital (ratio (:num roc) (:den roc))
             :equity (:den roc)
             :roc-end (:end roc))
      (and eps (number? (:val eps)))
      (assoc :eps (:val eps)
             :eps-unit (:unit eps)
             :eps-end (:end eps))
      (and dps (number? (:val dps)))
      (assoc :dps (:val dps)
             :dps-end (:end dps))
      (and dps (:unit dps))
      (assoc :dps-unit (:unit dps))
      (number? debt) (assoc :debt debt :debt-end bs-end)
      (number? cash) (assoc :cash cash)
      (number? preferred) (assoc :preferred-stock preferred)
      (number? nci) (assoc :noncontrolling-interest nci)
      (:ebit ebit) (assoc :ebit (:ebit ebit) :ebit-end (:ebit-end ebit))
      (number? coverage) (assoc :dividend-coverage coverage))))

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
    (throw (ex-info "Missing universe file; run clj -X:universe-facts first"
                    {:path universe/universe-path})))
  (edn/read-string (slurp universe/universe-path)))

(defn read-cik-json
  [zf cik]
  (when cik
    (let [entry (.getEntry zf (str "CIK" cik ".json"))]
      (when entry
        (with-open [in (.getInputStream zf entry)]
          (json/read-value in json/keyword-keys-object-mapper))))))

(defn load-sic-index
  "Lookups from data/universe-fsds.edn: :by-cik and :by-ticker, each
  {:sic :sic-description}. Empty when that file is missing."
  []
  (let [f (io/file fsds/filer-info-path)]
    (if-not (.exists f)
      (do (println (format "  no %s; sic left blank" fsds/filer-info-path))
          {:by-cik {} :by-ticker {}})
      (reduce (fn [idx row]
                (let [sic (some-> (:sic row) str str/trim not-empty)
                      desc (some-> (:sic-description row) str str/trim not-empty)
                      info (cond-> {}
                             sic (assoc :sic sic)
                             desc (assoc :sic-description desc))
                      cik (some-> (:cik row) str str/trim not-empty)
                      ticker (some-> (:ticker row) str str/trim not-empty str/upper-case)]
                  (cond-> idx
                    (and cik (seq info)) (update :by-cik assoc cik info)
                    (and ticker (seq info)) (update :by-ticker assoc ticker info))))
              {:by-cik {} :by-ticker {}}
              (edn/read-string (slurp f))))))

(defn with-sic
  "Copy :sic and :sic-description from universe-fsds onto a stats row.
  CIK wins; ticker is the fallback."
  [st {:keys [by-cik by-ticker]}]
  (let [info (or (get by-cik (some-> (:cik st) str str/trim))
                 (get by-ticker (some-> (:ticker st) str str/trim str/upper-case)))]
    (cond-> st
      (:sic info) (assoc :sic (:sic info))
      (:sic-description info) (assoc :sic-description (:sic-description info)))))

(def stats-field-order
  [:ticker :exchange :cik :entityName :sic :sic-description :price
   :shares :marketcap
   :revenue :revenue-unit
   :net-profit
   :eps
   :assets
   :return-on-assets
   :equity
   :return-on-capital
   :sales-growth-yoy
   :revenue-score
   :operating-income-score
   :price-earnings
   :dividend-yield
   :dividend-coverage
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

(defn merge-sic!
  "Add :sic and :sic-description from universe-fsds.edn onto the current
  data/stats.edn, without rebuilding fundamentals."
  []
  (let [rows (mapv #(with-sic % (load-sic-index))
                   (edn/read-string (slurp stats-path)))]
    (save-stats! rows)
    (println (format "Wrote %s  (%d companies; sic=%d  sic-description=%d)"
                     stats-path
                     (count rows)
                     (count (filter :sic rows))
                     (count (filter :sic-description rows))))
    rows))

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
  "From data/universe-facts.edn + companyfacts.zip + prices.edn, write data/stats.edn
  with 7-year mean YoY sales growth (%), profit margin, 10-year mean
  profit margin (Net Income / revenue), return on assets (Net Income / assets),
  return on capital (Net Income / total equity), plus :shares (most recent 10-Q
  when that quarter is at least as recent as the annual report; otherwise
  the annual report), :price, :marketcap, :price-sales,
  :price-earnings (USD price / USD EPS, else USD market cap / USD Net Income),
  :dividend-coverage (net income / dividends paid; omitted when none was paid),
  :enterprise-value (USD market cap + debt + preferred + NCI − cash),
  :ev-ebit, :revenue-score, and :operating-income-score
  (0–100 share of annual years that did not fall versus the prior year).
  When operating income is not tagged, a year whose operating expenses were
  derived uses gross profit minus those expenses.
  :sic and :sic-description come from data/universe-fsds.edn.

  Usage: clj -X:stats
         clj -X:stats :limit 25"
  ([] (stats {}))
  ([{:keys [limit force]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         all (load-universe)
         prices (load-price-by-ticker)
         sic-index (load-sic-index)
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
                 shares (or (when data
                              (universe/shares-outstanding (:facts data)))
                            (:shares-outstanding row))
                 st (with-sic
                      (try
                        (-> (if data
                              (company-stats (:facts data) row)
                              (select-keys row [:ticker :exchange :cik :entityName]))
                            (with-valuation prices shares))
                        (catch Exception ex
                          (println (format "  stats-fail %s (%s)" (:ticker row) (.getMessage ex)))
                          (select-keys row [:ticker :exchange :cik :entityName])))
                      sic-index)
                 i' (inc i)]
             (when (or (zero? (mod i' 250)) (= i' total))
               (println (format "  %d/%d  %s  growth=%s  margin=%s  roa=%s  roc=%s"
                                i' total
                                (or (:ticker st) (:cik st))
                                (fmt-growth (:sales-growth-yoy st))
                                (some-> (:profit-margin st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-assets st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-capital st) (* 100) (#(format "%.1f" %)))))
               (flush))
             (recur i' (conj! acc st) (next rows)))
           (let [out (vec (persistent! acc))]
             (save-stats! out)
             (println)
             (pprint/print-table
              [:ticker :price :marketcap :price-sales :price-earnings
               :sales-growth-yoy :profit-margin :return-on-assets :return-on-capital]
              (take 25
                    (map (fn [r]
                           (-> r
                               (update :sales-growth-yoy round1)
                               (update :profit-margin round4)
                               (update :return-on-assets round4)
                               (update :return-on-capital round4)
                               (update :price-sales round4)
                               (update :price-earnings round4)))
                         out)))
             (println (format "Wrote %s  (%d companies; growth=%d  margin=%d  avg-margin=%d  roa=%d  roc=%d  price=%d  mcap=%d  pe=%d  ev=%d  ev-ebit=%d  div-coverage=%d  rev-score=%d  oi-score=%d  sic=%d)"
                              stats-path
                              (count out)
                              (count (filter :sales-growth-yoy out))
                              (count (filter :profit-margin out))
                              (count (filter :avg-profit-margin out))
                              (count (filter :return-on-assets out))
                              (count (filter :return-on-capital out))
                              (count (filter :price out))
                              (count (filter :marketcap out))
                              (count (filter :price-earnings out))
                              (count (filter :enterprise-value out))
                              (count (filter :ev-ebit out))
                              (count (filter :dividend-coverage out))
                              (count (filter :revenue-score out))
                              (count (filter :operating-income-score out))
                              (count (filter :sic out))))
             out)))))))
