(ns edgar2.financials.calc.annual
  "Annual fundamentals from a companyfacts dataset."
  (:require [tech.v3.dataset :as ds]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.derived :as derived]
            [edgar2.financials.calc.concept :as calc-concept]
            [edgar2.financials.calc.quarterly :as calc-q]
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

(def sales-growth-window
  "Number of trailing YoY revenue changes averaged into :sales-growth-yoy."
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
                total-equity-tags
                parent-equity-tags
                nci-tags
                ebit-tags
                interest-expense-tags
                income-tax-tags
                cash-tag-pairs
                preferred-tags
                debt-tags
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

(defn avg-sales-growth
  "Mean of the last `n` year-over-year revenue changes (%).

  `vals` is oldest-first revenue numbers."
  [vals n]
  (let [growths (vec (keep identity (map yoy-pct vals (rest vals))))
        window (take-last n growths)]
    (when (seq window)
      {:sales-growth-yoy (avg window)
       :sales-growth-years (count window)})))

(defn latest-num
  "Newest numeric value and its end from parallel newest-first vectors."
  [ends vals]
  (some (fn [[e v]] (when (number? v) {:end e :val v}))
        (map vector ends vals)))

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

(defn most-recent
  "First numeric value in a newest-first vector."
  [vals]
  (some #(when (number? %) %) vals))

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
        ends (mapv #(str (:end %)) rows)
        current (first rows)
        annual-count (ds/row-count (facts-ds/annual-periods dss))
        concepts (calc-concept/calc-concepts table)
        rev (col-vec concepts :revenue)
        ni (col-vec concepts :net-income)
        assets (col-vec concepts :assets)
        oi (col-vec concepts :operating-income)
        equity (col-vec concepts :total-equity)
        debt-v (col-vec concepts :debt)
        cash-v (col-vec concepts :cash)
        preferred-v (col-vec concepts :preferred-stock)
        nci-v (col-vec concepts :nci)
        eps-v (col-vec concepts :eps)
        dps-v (col-vec concepts :dps)
        ebit-v (col-vec concepts :ebit)
        concept-rows (vec (ds/rows concepts {:nil-missing? true}))
        bs-end (:end (latest-num ends assets))
        bs-i (or (some identity
                       (map-indexed (fn [i e] (when (= e bs-end) i)) ends))
                 0)
        debt (nth debt-v bs-i nil)
        cash (nth cash-v bs-i nil)
        preferred (nth preferred-v bs-i nil)
        nci (nth nci-v bs-i nil)
        eps (most-recent eps-v)
        dps (or (most-recent dps-v) 0.0)
        ebit (most-recent ebit-v)
        growth (avg-sales-growth (chron-nums rev) sales-growth-window)
        ni-qoy (calc-q/net-income-change-q-on-y
                (facts-ds/report-table dss :q net-income-tags))
        margins (avg-profit-margin ni rev profit-margin-window)
        rev-score (direction-score (chron-nums rev))
        oi-score (direction-score (chron-nums oi))
        margin (most-recent (ratio ni rev))
        roa (most-recent (ratio ni assets))
        roc (most-recent (ratio ni equity))
        net-profit (most-recent ni)
        revenue (most-recent rev)
        assets-val (most-recent assets)
        equity-val (most-recent equity)
        div (div-cov/calculate-dividend-coverage
             (or (first rows) current)
             (or (first concept-rows) {}))]
    (cond-> (merge {:annual-count annual-count} div)
      (:sales-growth-yoy growth)
      (assoc :sales-growth-yoy (:sales-growth-yoy growth)
             :sales-growth-years (:sales-growth-years growth))
      (number? ni-qoy) (assoc :net-income-change-q-on-y ni-qoy)
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
      (number? roc)
      (assoc :return-on-capital roc
             :equity equity-val)
      (number? eps) (assoc :eps eps)
      (number? dps) (assoc :dps dps)
      (number? debt) (assoc :debt debt :debt-end bs-end)
      (number? cash) (assoc :cash cash)
      (number? preferred) (assoc :preferred-stock preferred)
      (number? nci) (assoc :noncontrolling-interest nci)
      (number? ebit) (assoc :ebit ebit))))
