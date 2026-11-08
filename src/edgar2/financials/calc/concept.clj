(ns edgar2.financials.calc.concept
  "Per-period concept vectors from an annual (or quarterly) report-table."
  (:require
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.financials.calc.math :as math]
   [tech.v3.dataset :as ds]
   [tablecloth.api :as tc]))

;; concepts

(def concept-ids
  "Concept :ids whose per-period vectors are added by `calc-concepts`.

  The income-statement inputs `:operating-expenses`, `:pretax-income`,
  `:nonoperating-income`, and `:asset-gains` are included so
  `calc.math/calculate-math` can derive the operating lines. After
  `calculate-math`, `:operating-expenses` and `:operating-income` are
  reported-else-derived (see `calc.math`)."
  [; pl
   :revenue :cost-of-revenue :gross-profit
   :operating-income
   :net-income
   :ebit
   :depreciation-amortization
   :depreciation
   :amortization
   :interest-expense
   :income-tax
   ; pl derivation inputs
   :operating-expenses :pretax-income :nonoperating-income :asset-gains
   ; assets/liabilities
   :assets :assets-current :liabilities-current
   :total-equity :debt-longterm :debt-shortterm
   :cash :preferred-stock :nci :parent-equity
   ; payout
   :dividends-paid :stock-repurchase])

(defn add-concept [report-ds concept-ds concept]
  (tc/add-column
   concept-ds
   concept
   (facts-ds/calc-concept report-ds concept)))

(defn calc-concept-ds [report-ds]
  (let [concept-ds (ds/select-columns report-ds [:filed :period :end])]
    (reduce (partial add-concept report-ds) concept-ds concept-ids)))

(defn calc-concepts
  "Wide dataset of calculated concepts aligned with `ds-annual`.
  Keeps `:filed :period :end` from the report-table. Adds one column per
  entry in `concept-ids` (column name = concept `:id`). Values are
  newest-first, same row order as `ds-annual`. `:dividends-paid` and
  `:stock-repurchase` are absolute (positive) amounts. `:ebit` prefers
  operating income, else net income + interest + tax. `:operating-expenses`
  is the reported tag else the income-statement identity, and
  `:operating-income` is the reported tag else gross profit minus a derived
  operating-expenses (see `calc.math/calculate-math`)."
  [annual-ds]
  (let [concept-ds (calc-concept-ds annual-ds)]
    (-> concept-ds
        (math/calculate-math)
        (math/calculate-ratios))))
