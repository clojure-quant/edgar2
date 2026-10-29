(ns edgar2.financials.facts-ui
  "Which rows the financials page shows.

  A field map is one of:
    :field     one [taxonomy concept] pair
    :concept   an id from edgar2.financials.concepts, via facts-ds/calc-concept
    :edgarjure a statement label from edgarjure
    :math      a key in edgar2.financials.math/math

  :style and :format are the row background (:background-pl-green,
  :background-balance-active, or :background-balance-passive), written on
  the field map and carried as column metadata. :section is :income or
  :balance.

  Views: :current (SIC-routed statement), :all (every concept on a wide
  report-table), :insurance, :bank, :reit (that industry's income lines,
  plus the shared balance-sheet lines and dividend / repurchase rows).

  Each view's rows are a static vector of field maps, already in display
  order. `statement` returns a wide period-row dataset; `format-columns`,
  `to-presentable`, and `remove-presentable-empty` prepare it for display."
  (:require [clojure.string :as str]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as col]))

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

(def view-choices
  [[:current "Current"]
   [:all "All fields"]
   [:insurance "Insurance"]
   [:bank "Bank"]
   [:reit "REIT"]])

(def pl-green-labels
  #{"Gross Profit" "Operating Income" "Net Income"
    "Dividends Paid" "Stock Repurchase"
    "Net Interest Income" "Net Interest Income After Provision"
    "Premiums Earned"})

(def asset-labels
  #{"Cash and Equivalents"
    "Accounts Receivable"
    "Inventory"
    "Current Assets"
    "PP&E Net"
    "Total Assets"})

(def passive-labels
  #{"Accounts Payable"
    "Current Debt"
    "Current Portion of Long-Term Debt"
    "Short-Term Borrowings"
    "Current Liabilities"
    "Long-Term Debt"
    "Total Liabilities"
    "Stockholders Equity"
    "Total Equity"
    "Total Liabilities and Equity"})

(defn parse-view
  "One of `view-choices`, or :current."
  [view]
  (let [s (str/lower-case
           (cond
             (nil? view) "current"
             (keyword? view) (name view)
             :else (str view)))
        k (keyword s)]
    (if (some #(= k (first %)) view-choices) k :current)))

(defn view-label
  [view]
  (or (second (first (filter #(= (parse-view view) (first %)) view-choices)))
      "Current"))

(defn format-for
  "Background keyword for a statement label, or nil."
  [label]
  (cond
    (pl-green-labels label) :background-pl-green
    (asset-labels label) :background-balance-active
    (passive-labels label) :background-balance-passive))

(def ^:private income-labels
  "edgarjure income line labels, by industry."
  {:standard ["Revenue"
              "Cost of Revenue"
              "Gross Profit"
              "Operating Expenses"
              "Total Costs and Expenses"
              "R&D Expense"
              "SG&A Expense"
              "Selling and Marketing Expense"
              "General and Administrative Expense"
              "Marketing Expense"
              "Other Operating Expense"
              "Excise Taxes"
              "Restructuring Charges"
              "Impairment Charges"
              "Goodwill Impairment"
              "Acquisition-Related Costs"
              "Litigation Settlement"
              "Gain (Loss) on Sale of Assets"
              "In-Process R&D"
              "Operating Income"
              "Non-Operating Income"
              "Interest Expense"
              "Pre-Tax Income"
              "Income Tax Expense"
              "Net Income"
              "EPS Basic"
              "EPS Diluted"
              "Shares Basic"
              "Shares Diluted"]
   :bank ["Interest Income"
          "Interest Expense"
          "Net Interest Income"
          "Provision for Credit Losses"
          "Net Interest Income After Provision"
          "Noninterest Income"
          "Noninterest Expense"
          "Total Revenue"
          "Total Net Revenue"
          "Compensation Expense"
          "Occupancy Expense"
          "Pre-Tax Income"
          "Income Tax Expense"
          "Net Income"
          "EPS Basic"
          "EPS Diluted"
          "Shares Basic"
          "Shares Diluted"]
   :insurance ["Premiums Earned"
               "Net Investment Income"
               "Realized Investment Gains"
               "Total Revenue"
               "Policyholder Benefits and Claims"
               "Interest Credited to Policyholders"
               "Policyholder Dividends"
               "Acquisition Cost Amortization"
               "Total Benefits and Expenses"
               "Pre-Tax Income"
               "Income Tax Expense"
               "Net Income"
               "EPS Basic"
               "EPS Diluted"
               "Shares Basic"
               "Shares Diluted"]
   :reit ["Revenue"
          "Rental Revenue"
          "Real Estate Revenue"
          "Tenant Reimbursements"
          "Other Property Revenue"
          "Property Operating Expense"
          "Real Estate Taxes"
          "Ground Rent Expense"
          "General and Administrative Expense"
          "SG&A Expense"
          "D&A"
          "Impairment of Real Estate"
          "Total Costs and Expenses"
          "Gain on Sale of Real Estate"
          "Operating Income"
          "Non-Operating Income"
          "Interest Expense"
          "Pre-Tax Income"
          "Income Tax Expense"
          "Net Income"
          "EPS Basic"
          "EPS Diluted"
          "Shares Basic"
          "Shares Diluted"]})

(defn- with-style
  [field]
  (if-let [style (format-for (:label field))]
    (assoc field :style style)
    field))

(defn- edgarjure-line
  [section label]
  (with-style {:edgarjure label :label label :section section}))

(defn- concept-line
  [id label]
  (with-style {:concept id :label label :section :income}))

(defn- math-line
  [kw label]
  (with-style {:math kw :label label :section :income}))

(def ^:private cash-lines
  [(math-line :dividends-paid "Dividends Paid")
   (math-line :stock-repurchase "Stock Repurchase")])

(defn- order-fields
  "Labels in `preferred` first, then the remaining fields in their current order."
  [fields preferred]
  (let [by (into {} (map (juxt :label identity) fields))
        seen (set preferred)
        head (keep by preferred)
        tail (remove #(seen (:label %)) fields)]
    (vec (concat head tail))))

(defn- current-line
  "Current view: revenue, depreciation, and the calculated operating and
  cash lines. Every other label stays an edgarjure chain."
  [label]
  (case label
    "Revenue" (concept-line :revenue "Revenue")
    "Operating Expenses" (math-line :operating-expenses "Operating Expenses")
    "Operating Income" (math-line :operating-income "Operating Income")
    (edgarjure-line :income label)))

(defn- append-missing
  [lines extras]
  (let [have (set (map :label lines))]
    (into lines (remove #(have (:label %)) extras))))

(defn- current-income
  [industry]
  (let [labels (get income-labels industry (:standard income-labels))
        lines (append-missing (mapv current-line labels)
                              [(concept-line :depreciation "Depreciation")])]
    (order-fields (append-missing lines cash-lines) pl-column-order)))

(defn- industry-income
  [industry]
  (append-missing (mapv #(edgarjure-line :income %) (get income-labels industry))
                  cash-lines))

(def ^:private balance-fields
  [{:edgarjure "Cash and Equivalents"
    :label "Cash and Equivalents"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Accounts Receivable"
    :label "Accounts Receivable"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Inventory"
    :label "Inventory"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Current Assets"
    :label "Current Assets"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "PP&E Net"
    :label "PP&E Net"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Total Assets"
    :label "Total Assets"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Accounts Payable"
    :label "Accounts Payable"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Current Debt"
    :label "Current Debt"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Current Portion of Long-Term Debt"
    :label "Current Portion of Long-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Short-Term Borrowings"
    :label "Short-Term Borrowings"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Current Liabilities"
    :label "Current Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Long-Term Debt"
    :label "Long-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Total Liabilities"
    :label "Total Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Stockholders Equity"
    :label "Stockholders Equity"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Total Equity"
    :label "Total Equity"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Total Liabilities and Equity"
    :label "Total Liabilities and Equity"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}])

(def ^:private current-income-fields
  {:standard (current-income :standard)
   :bank (current-income :bank)
   :insurance (current-income :insurance)
   :reit (current-income :reit)})

(def ^:private industry-income-fields
  {:bank (industry-income :bank)
   :insurance (industry-income :insurance)
   :reit (industry-income :reit)})

(defn view-fields
  "Field specs for `view`. `industry` is the company SIC route (:standard,
  :bank, :insurance, :reit) and applies to :current. :all has no specs."
  [view industry]
  (case (parse-view view)
    :all []
    :insurance (vec (concat (:insurance industry-income-fields) balance-fields))
    :bank (vec (concat (:bank industry-income-fields) balance-fields))
    :reit (vec (concat (:reit industry-income-fields) balance-fields))
    (vec (concat (get current-income-fields (or industry :standard)
                      (:standard current-income-fields))
                 balance-fields))))

(def ^:private period-col-names
  (set facts-ds/report-period-cols))

(def ^:private presentable-meta-cols
  #{:field :style :format :section})

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn- eps-column?
  [name]
  (boolean (re-find #"(?i)eps|per\s*share" (str name))))

(defn- scale-display
  "USD and share counts → millions. Per-share amounts stay in dollars."
  [name v]
  (when (number? v)
    (if (eps-column? name)
      (double v)
      (long (/ (double v) 1.0e6)))))

(defn- field-column-names
  [ds]
  (into []
        (remove #(contains? period-col-names %))
        (ds/column-names ds)))

(defn- make-column
  [name values metadata]
  (let [values (vec values)
        data (if (empty? values) (make-array Object 0) values)]
    (col/new-column name data metadata)))

(defn format-columns
  "Scale every field column on a wide statement dataset to millions.

  Leaves `:filed :period :end` alone. EPS / per-share columns stay in
  dollars. Column metadata is kept."
  [ds]
  (let [columns (mapv (fn [name]
                        (let [c (ds/column ds name)
                              metadata (dissoc (meta c) :name :datatype :n-elems)]
                          (if (contains? period-col-names name)
                            (make-column name (vec c) metadata)
                            (make-column name
                                         (mapv #(scale-display name %) c)
                                         metadata))))
                      (ds/column-names ds))]
    (ds/new-dataset {:dataset-name (or (ds/dataset-name ds) "report-table")}
                    columns)))

(defn to-presentable
  "Transpose a wide period-row statement into a field-row table.

  Period `:end` values become column names in row order (`:field` first).
  Field column order is unchanged. Adds `:style` (from column `:style` or
  `:format`), plus `:section` and `:format` when present. Keeps all-nil
  field rows."
  [ds]
  (let [rows (vec (ds/rows ds {:nil-missing? true}))
        ends (mapv #(str (:end %)) rows)
        field-names (field-column-names ds)
        present-rows
        (mapv (fn [name]
                (let [c (ds/column ds name)
                      metadata (meta c)
                      style (or (:style metadata) (:format metadata))
                      period-vals (mapv (fn [row] (as-number (get row name))) rows)]
                  (cond-> (into {:field (str name)}
                                (map vector ends period-vals))
                    style (assoc :style style)
                    (:format metadata) (assoc :format (:format metadata))
                    (:section metadata) (assoc :section (:section metadata)))))
              field-names)
        ;; ds/->dataset ignores :column-order for map rows; select-columns
        ;; is what actually pins :field then period ends in row order.
        meta-cols (filterv (fn [k]
                             (some #(contains? % k) present-rows))
                           [:style :format :section])
        col-order (into (into [:field] ends) meta-cols)
        table (ds/->dataset present-rows {:dataset-name "presentable"})]
    (if (seq present-rows)
      (ds/select-columns table col-order)
      table)))

(defn remove-presentable-empty
  "Drop presentable rows where every period value is non-numeric."
  [ds]
  (let [period-cols (into []
                          (remove #(contains? presentable-meta-cols %))
                          (ds/column-names ds))]
    (ds/filter ds
               (fn [row]
                 (some #(number? (get row %)) period-cols)))))

(defn presentable
  "Wide statement → scaled, transposed, empty rows removed."
  [ds]
  (-> ds format-columns to-presentable remove-presentable-empty))

(defn statement
  "Wide report-table for `view` over annual (`:y`) or quarterly (`:q`).

  Curated views use `report-table-full` with `view-fields`. `:all` uses
  `report-table` with no field filter (same orientation, no style metadata).
  Rows are newest `:end` first."
  [dss q-or-y view industry]
  (let [view (parse-view view)]
    (if (= view :all)
      (facts-ds/report-table dss q-or-y nil)
      (facts-ds/report-table-full dss q-or-y (view-fields view industry)))))
