(ns edgar2.financials.calc.annual
  "Annual fundamentals from a companyfacts dataset."
  (:require [tech.v3.dataset :as ds]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.derived :as derived]
            [edgar2.financials.calc.concept :as calc-concept]
            [edgar2.financials.calc.quarterly :as calc-q]
            [edgar2.financials.calc.regression :as regression]
            [edgar2.financials.calc.row.dividend-coverage :as div-cov]
            [edgar2.financials.facts-ds :as facts-ds]))

(defn- concept-data
  [id]
  (:data (concepts/get-concept-id id)))

(def net-income-tags (concept-data :net-income))
(def eps-tags (concept-data :eps))
(def dps-tags (concept-data :dps))
(def dividends-paid-tags (concept-data :dividends-paid))
(def stock-repurchase-tags (concept-data :stock-repurchase))
(def asset-tags (concept-data :assets))
(def assets-current-tags (concept-data :assets-current))
(def liabilities-current-tags (concept-data :liabilities-current))
(def total-equity-tags (concept-data :total-equity))
(def parent-equity-tags (concept-data :parent-equity))
(def nci-tags (concept-data :nci))
(def ebit-tags (concept-data :operating-income))
(def interest-expense-tags (concept-data :interest-expense))
(def income-tax-tags (concept-data :income-tax))
(def cash-tag-pairs (concept-data :cash))
(def preferred-tags (concept-data :preferred-stock))
(def debt-tags (concept-data :debt))
(def revenue-tags (concept-data :revenue))
(def depreciation-tags (concept-data :depreciation))
(def amortization-tags (concept-data :amortization))

(def sales-growth-window
  "Trailing years averaged into :sales-growth-avg-yoy. The same count of
  annual net-income slots, oldest first, is passed to the earnings regression."
  7)

(def profit-margin-window
  "Number of trailing annual profit margins averaged into :avg-profit-margin."
  10)

(def annual-fields
  "All concept pairs pulled into the annual report-table."
  (vec (distinct
        (concat revenue-tags
                net-income-tags
                eps-tags
                dps-tags
                dividends-paid-tags
                stock-repurchase-tags
                asset-tags
                assets-current-tags
                liabilities-current-tags
                total-equity-tags
                parent-equity-tags
                nci-tags
                ebit-tags
                interest-expense-tags
                income-tax-tags
                cash-tag-pairs
                preferred-tags
                debt-tags
                depreciation-tags
                amortization-tags
                (mapcat second derived/statement-lines)))))

(defn- yoy-pct
  [prev cur]
  (when (and (number? prev) (number? cur) (not (zero? (double prev))))
    (* 100.0 (/ (- (double cur) (double prev)) (double prev)))))

(defn- avg
  [xs]
  (when (seq xs)
    (/ (double (reduce + xs)) (count xs))))

(defn direction-score
  "Share of years that did not fall versus the prior year, from 0 to 100.

  `vals` is oldest-first numbers. Each value after the first scores 1 when
  value − prior >= 0, otherwise 0. The score is 100 times that count divided
  by the number of year-to-year steps. The first year has no prior, so it is
  omitted."
  [vals]
  (let [steps (keep (fn [[prev cur]]
                      (when (and (number? prev) (number? cur))
                        (if (>= (double cur) (double prev)) 1 0)))
                    (map vector vals (rest vals)))]
    (when (seq steps)
      (* 100.0 (/ (double (reduce + steps)) (count steps))))))

(defn- avg-trailing-change
  "Mean of the last `n` period-over-period changes.

  `vals` is oldest-first. `change` is a fn of [prev cur] returning a number
  or nil. Returns {:avg :years}, or nil when no changes remain."
  [vals n change]
  (let [growths (vec (keep identity (map change vals (rest vals))))
        window (take-last n growths)]
    (when (seq window)
      {:avg (avg window)
       :years (count window)})))

(defn avg-sales-growth
  "Mean of the last `n` year-over-year revenue changes (%).

  `vals` is oldest-first revenue numbers."
  [vals n]
  (when-let [{:keys [avg years]} (avg-trailing-change vals n yoy-pct)]
    {:sales-growth-avg-yoy avg
     :sales-growth-years years}))

(defn chron-nums
  "Oldest-first numbers from a newest-first `calc-concept` vector."
  [vals]
  (->> vals reverse (filterv number?)))

(defn- table-rows
  [table]
  (vec (ds/rows table {:nil-missing? true})))

(defn ratio-row
  "num/den for one period. Nil when either side is missing or den is zero."
  [num den]
  (when (and (number? num) (number? den) (not (zero? (double den))))
    (/ (double num) (double den))))

(defn ratio
  "Per-row num/den from parallel newest-first vectors (row-aligned).

  Returns a newest-first vector; nil where either side is missing or the
  denominator is zero."
  [nums dens]
  (mapv ratio-row nums dens))

(defn- capital-employed
  "Per-period assets minus current liabilities.

  Nil where either side is missing. A missing current-liabilities figure
  is not treated as zero."
  [assets liabilities]
  (mapv (fn [a l]
          (when (and (number? a) (number? l))
            (- (double a) (double l))))
        assets liabilities))

(defn most-recent
  "First numeric value in a newest-first vector."
  [vals]
  (some #(when (number? %) %) vals))

(def ^:private combined-da-order
  "Cash-flow or statement totals that already include depreciation and
  amortization. Earlier names win."
  ["DepreciationDepletionAndAmortization"
   "DepreciationAndAmortization"
   "DepreciationAndAmortisationExpense"
   "AdjustmentsForDepreciationAndAmortisationExpense"])

(def ^:private combined-da-tags (set combined-da-order))

(defn- tag-names
  [pairs]
  (mapv (comp name second) pairs))

(defn- first-num
  "First numeric column in `names` on one report-table row."
  [row names]
  (some (fn [n]
          (let [v (get row n)]
            (when (number? v) v)))
        names))

(defn- depreciation-and-amortization
  "Newest-first [depreciation amortization] from a report-table.

  A combined D&A tag is returned as depreciation and amortization is nil
  on that row, so adding the vectors does not count amortization twice.
  Otherwise each concept contributes its own first numeric tag."
  [table]
  (let [dep-names (tag-names depreciation-tags)
        pure-dep (filterv (complement combined-da-tags) dep-names)
        amort-names (tag-names amortization-tags)
        rows (ds/rows table {:nil-missing? true})]
    [(mapv (fn [row]
             (or (first-num row combined-da-order)
                 (first-num row pure-dep)))
           rows)
     (mapv (fn [row]
             (when-not (number? (first-num row combined-da-order))
               (first-num row amort-names)))
           rows)]))

(defn ebitda-vec
  "Per-row EBITDA, newest first: EBIT + depreciation + amortization.

  Nil when EBIT is missing or both addends are missing. A missing addend
  counts as zero."
  [ebit dep amort]
  (mapv (fn [e d a]
          (when (and (number? e) (or (number? d) (number? a)))
            (+ (double e)
               (double (or d 0))
               (double (or a 0)))))
        ebit dep amort))

(defn yearly-ratios
  "num/den per row-aligned period, oldest first.

  `nums` and `dens` are newest-first value vectors from the same dataset.
  Periods where either side is missing, or the denominator is zero, are
  omitted."
  [nums dens]
  (->> (ratio nums dens) reverse (filterv number?)))

(defn avg-profit-margin
  "Mean of the last `n` annual profit margins (net income / revenue).

  `ni` and `rev` are newest-first value vectors."
  [ni rev n]
  (let [window (take-last n (yearly-ratios ni rev))]
    (when (seq window)
      {:avg-profit-margin (avg window)
       :avg-profit-margin-years (count window)})))

(defn- col-vec
  "Values of column `k` on `ds` as a vector (empty when missing)."
  [dss k]
  (if (ds/has-column? dss k)
    (vec (ds/column dss k))
    []))

(defn calculate-annual
  "Fundamentals from an `e/facts`-shaped dataset via `report-table`.

  First table row is the current year; trailing rows feed growth and
  average margins."
  [dss]
  (let [table (facts-ds/report-table dss :y annual-fields)
        rows (table-rows table)
        current (first rows)
        annual-count (ds/row-count (facts-ds/annual-periods dss))
        concepts (calc-concept/calc-concepts table)
        rev (col-vec concepts :revenue)
        ni (col-vec concepts :net-income)
        assets (col-vec concepts :assets)
        liabilities-current (col-vec concepts :liabilities-current)
        oi (col-vec concepts :operating-income)
        equity (col-vec concepts :total-equity)
        debt-v (col-vec concepts :debt)
        cash-v (col-vec concepts :cash)
        preferred-v (col-vec concepts :preferred-stock)
        nci-v (col-vec concepts :nci)
        eps-v (col-vec concepts :eps)
        dps-v (col-vec concepts :dps)
        ebit-v (col-vec concepts :ebit)
        [dep-v amort-v] (depreciation-and-amortization table)
        ebitda-v (ebitda-vec ebit-v dep-v amort-v)
        concept-rows (vec (ds/rows concepts {:nil-missing? true}))
        debt (most-recent debt-v)
        cash (most-recent cash-v)
        preferred (most-recent preferred-v)
        nci (most-recent nci-v)
        eps (most-recent eps-v)
        dps (or (most-recent dps-v) 0.0)
        ebit (most-recent ebit-v)
        ebitda (most-recent ebitda-v)
        debt-v-div-ebitda (ratio debt-v ebitda-v)
        debt2ebitda (most-recent debt-v-div-ebitda)
        growth (avg-sales-growth (chron-nums rev) sales-growth-window)
        earn (regression/earnings-regression
              (take-last sales-growth-window (reverse ni)))
        ni-qoy (calc-q/net-income-change-q-on-y
                (facts-ds/report-table dss :q net-income-tags))
        rev-qoy (calc-q/sales-change-q-on-y
                 (facts-ds/report-table dss :q revenue-tags))
        margins (avg-profit-margin ni rev profit-margin-window)
        rev-score (direction-score (chron-nums rev))
        oi-score (direction-score (chron-nums oi))
        margin (most-recent (ratio ni rev))
        roa (most-recent (ratio ni assets))
        roce (most-recent (ratio ebit-v (capital-employed assets liabilities-current)))
        net-profit (most-recent ni)
        revenue (most-recent rev)
        assets-val (most-recent assets)
        equity-val (most-recent equity)
        div (div-cov/calculate-dividend-coverage
             (or (first rows) current)
             (or (first concept-rows) {}))]
    (cond-> (merge {:annual-count annual-count} div)
      (:sales-growth-avg-yoy growth)
      (assoc :sales-growth-avg-yoy (:sales-growth-avg-yoy growth)
             :sales-growth-years (:sales-growth-years growth))
      earn (merge earn)
      (number? ni-qoy) (assoc :net-income-change-q-on-y ni-qoy)
      (number? rev-qoy) (assoc :sales-change-q-on-y rev-qoy)
      (number? rev-score) (assoc :revenue-score rev-score)
      (number? oi-score) (assoc :operating-income-score oi-score)
      (number? margin)
      (assoc :profit-margin margin
             :net-profit net-profit
             :revenue revenue)
      (:avg-profit-margin margins)
      (assoc :avg-profit-margin (:avg-profit-margin margins)
             :avg-profit-margin-years (:avg-profit-margin-years margins))
      (number? roa)
      (assoc :return-on-assets roa
             :assets assets-val)
      (number? equity-val) (assoc :equity equity-val)
      (number? roce) (assoc :return-on-capital-employed roce)
      (number? eps) (assoc :eps eps)
      (number? dps) (assoc :dps dps)
      (number? debt) (assoc :debt debt)
      (number? cash) (assoc :cash cash)
      (number? preferred) (assoc :preferred-stock preferred)
      (number? nci) (assoc :noncontrolling-interest nci)
      (number? ebit) (assoc :ebit ebit)
      (number? ebitda) (assoc :ebitda ebitda)
      (number? debt2ebitda) (assoc :debt2ebitda debt2ebitda))))
