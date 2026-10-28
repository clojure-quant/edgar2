(ns edgar2.financials.facts-ui
  "Which rows the financials page shows.

  A field is one of three kinds:
    :raw       one companyfacts concept (taxonomy, concept, unit)
    :edgarjure a fallback chain from edgarjure (e/concepts-for)
    :edgar2    an id from edgar2.financials.concepts, via facts-ds/calc-concept

  :format is the row background: :background-pl-green,
  :background-balance-active, or :background-balance-passive.

  Views: :current (SIC-routed statement), :all (every concept select-data
  keeps), :insurance, :bank, :reit (that industry's income chains, plus the
  shared balance-sheet lines and dividend / repurchase rows)."
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.financials.calc.concept :as calc]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.derived :as derived]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]))

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

(defn- with-format
  [field]
  (if-let [fmt (format-for (:label field))]
    (assoc field :format fmt)
    field))

(defn- local-names
  [pairs]
  (mapv (comp name second) pairs))

(defn- names-by-label
  [id label]
  (some-> (concepts/get-concept-id id) :data (get label) local-names))

(defn- prioritize-revenue
  "Headline totals before ASC 606 contract subsets."
  [concept-names]
  (let [preferred (local-names (:data (concepts/get-concept-id :revenue)))
        pri (set preferred)]
    (into (filterv (set concept-names) preferred)
          (remove pri concept-names))))

(defn- income-chains
  "[[label [concept-name ...]] ...] for an edgarjure income industry.
  `:current?` appends the depreciation chain and IFRS supplements."
  [industry current?]
  (let [chains (:chains (e/concepts-for :income :industry (or industry :standard)))
        lines (mapv (fn [[label & concept-names]]
                      (let [names (vec (concat concept-names
                                               (when current?
                                                 (names-by-label :ifrs-income-by-label label))
                                               (when current?
                                                 (names-by-label :income-fallbacks-by-label label))))]
                        [label (if (and current? (#{"Revenue" "Total Revenue"} label))
                                 (prioritize-revenue names)
                                 names)]))
                    chains)]
    (if current?
      (conj lines ["Depreciation" (local-names (:data (concepts/get-concept-id :depreciation)))])
      lines)))

(defn- chain-field
  [section label concept-names]
  (with-format {:kind :edgarjure
                :section section
                :label label
                :concepts (vec concept-names)}))

(def ^:private edgar2-income
  {"Operating Expenses" {:kind :edgar2
                         :section :income
                         :id :operating-expenses
                         :label "Operating Expenses"
                         :format :background-pl-green}
   "Operating Income" {:kind :edgar2
                       :section :income
                       :id :operating-income
                       :label "Operating Income"
                       :format :background-pl-green}})

(def cash-fields
  [{:kind :edgar2
    :section :income
    :id :dividends-paid
    :abs true
    :label "Dividends Paid"
    :format :background-pl-green}
   {:kind :edgar2
    :section :income
    :id :stock-repurchase
    :abs true
    :label "Stock Repurchase"
    :format :background-pl-green}])

(defn- order-fields
  "Labels in `preferred` first, then the remaining fields in their current order."
  [fields preferred]
  (let [by (into {} (map (juxt :label identity) fields))
        seen (set preferred)
        head (keep by preferred)
        tail (remove #(seen (:label %)) fields)]
    (vec (concat head tail))))

(defn- balance-fields
  []
  (let [by (into {} (map (fn [[label & concept-names]]
                           [label (vec concept-names)])
                         (:chains (e/concepts-for :balance))))]
    (into []
          (keep (fn [label]
                  (when-let [concept-names (get by label)]
                    (chain-field :balance label concept-names))))
          bs-column-order)))

(defn- income-fields
  [industry current?]
  (let [fields (mapv (fn [[label concept-names]]
                       (chain-field :income label concept-names))
                     (income-chains industry current?))
        fields (if current?
                 (mapv #(get edgar2-income (:label %) %) fields)
                 fields)
        have (set (map :label fields))
        divs (remove #(have (:label %)) cash-fields)]
    (if current?
      (order-fields (into fields divs) pl-column-order)
      (vec (concat fields divs)))))

(defn view-fields
  "Field specs for `view`. `industry` is the company SIC route (:standard,
  :bank, :insurance, :reit) and applies to :current. :all has no specs."
  [view industry]
  (case (parse-view view)
    :all []
    :insurance (vec (concat (income-fields :insurance false) (balance-fields)))
    :bank (vec (concat (income-fields :bank false) (balance-fields)))
    :reit (vec (concat (income-fields :reit false) (balance-fields)))
    (vec (concat (income-fields industry true) (balance-fields)))))

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn- jure-pairs
  [concept-names]
  (for [taxonomy [:us-gaap :ifrs-full :dei]
        concept concept-names]
    [taxonomy (keyword (str concept))]))

(defn- field-pairs
  "Concept pairs `report-table` must load for `fields`."
  [fields]
  (let [base (mapcat (fn [{:keys [kind concepts id]}]
                       (case kind
                         :edgarjure (jure-pairs concepts)
                         :edgar2 (let [data (:data (concepts/get-concept-id id))]
                                   (when (sequential? data) data))
                         []))
                     fields)
        derived? (some #(and (= :edgar2 (:kind %))
                             (#{:operating-expenses :operating-income} (:id %)))
                       fields)
        extra (when derived? (mapcat second derived/statement-lines))]
    (vec (distinct (concat base extra)))))

(defn- ends
  [table]
  (mapv #(str (:end %)) (ds/rows table {:nil-missing? true})))

(defn- by-end
  [table id]
  (into {}
        (keep (fn [[end v]]
                (when (number? v) [end v]))
              (map vector (ends table) (facts-ds/calc-concept table id)))))

(defn- operating-expenses-vec
  "Per-row operating expenses, reported or derived, newest first."
  [table]
  (let [es (ends table)
        by-item (into {}
                      (map (fn [id]
                             [(:label (concepts/get-concept-id id))
                              (by-end table id)]))
                      derived/statement-line-ids)
        filled (derived/with-derived-lines by-item es)]
    (mapv #(get (get filled derived/operating-expenses) %) es)))

(defn- calc-chain
  [table concept-names]
  (let [names (mapv str concept-names)]
    (mapv (fn [row]
            (some #(as-number (get row %)) names))
          (ds/rows table {:nil-missing? true}))))

(defn- line-vals
  [table {:keys [kind concepts id abs]}]
  (case kind
    :edgarjure (calc-chain table concepts)
    :edgar2 (cond
              abs (calc/concept-absolute table id)
              (= id :operating-income) (calc/operating-income table)
              (= id :operating-expenses) (operating-expenses-vec table)
              :else (facts-ds/calc-concept table id))
    []))

(defn- period-maps
  [table]
  (mapv (fn [row]
          {:end (str (:end row))
           :filed (str (:filed row))
           :period (:period row)})
        (ds/rows table {:nil-missing? true})))

(defn- curated-statement
  [dss q-or-y fields]
  (let [table (facts-ds/report-table dss q-or-y (field-pairs fields))]
    {:periods (period-maps table)
     :lines (mapv (fn [field]
                    (assoc field :vals (line-vals table field)))
                  fields)}))

(defn- raw-label
  [taxonomy concept unit]
  (if (str/blank? (str unit))
    (str taxonomy ":" concept)
    (str taxonomy ":" concept " (" unit ")")))

(defn- all-statement
  "`select-data` with no concept filter. One :raw row per taxonomy, concept, and unit."
  [dss q-or-y]
  (let [rows (->> (ds/rows (facts-ds/select-data dss q-or-y nil) {:nil-missing? true})
                  (remove #(str/blank? (str (:concept %))))
                  vec)
        periods (->> rows
                     (group-by #(str (:end %)))
                     (map (fn [[end facts]]
                            (let [fact (first facts)]
                              {:end end
                               :filed (str (:filed fact))
                               :period (:period fact)})))
                     (sort-by :end #(compare %2 %1))
                     vec)
        ends (mapv :end periods)]
    {:periods periods
     :lines (->> rows
                 (group-by (juxt #(str (:taxonomy %))
                                 #(str (:concept %))
                                 #(str (:unit %))))
                 (sort-by key)
                 (mapv (fn [[[taxonomy concept unit] facts]]
                         (let [best (reduce (fn [m fact]
                                              (let [end (str (:end fact))
                                                    cur (get m end)]
                                                (if (or (nil? cur)
                                                        (pos? (compare (str (:filed fact))
                                                                       (str (:filed cur)))))
                                                  (assoc m end fact)
                                                  m)))
                                            {}
                                            facts)]
                           {:kind :raw
                            :section :raw
                            :taxonomy taxonomy
                            :concept concept
                            :unit unit
                            :label (raw-label taxonomy concept unit)
                            :vals (mapv #(as-number (:val (get best %))) ends)})))
                 (filterv #(some number? (:vals %))))}))

(defn statement
  "Rows for `view` over annual (`:y`) or quarterly (`:q`) periods.

  `industry` is the SIC route used by :current. Returns
  `{:periods [{:end :filed :period} ...] :lines [...]}`.
  Periods and `:vals` are newest first."
  [dss q-or-y view industry]
  (if (= (parse-view view) :all)
    (all-statement dss q-or-y)
    (curated-statement dss q-or-y (view-fields view industry))))
