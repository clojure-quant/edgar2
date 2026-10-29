(ns edgar2.financials.math
  "Calculated statement lines for `report-table-full`.

  `math` maps a keyword to a pure calc, a `derive` (reported concept, else
  that calc), or an `absolute` concept. `calculate-math` returns one value
  per row of a wide report-table, newest first."
  (:refer-clojure :exclude [derive])
  (:require [edgar2.financials.concepts :as concepts]
            [edgar2.financials.derived :as derived]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]))

(defn derive
  "Reported `concept-id`, else the math entry `calc-kw`.

  Per row, a numeric `facts-ds/calc-concept` value wins."
  [concept-id calc-kw]
  {:reported concept-id :calc calc-kw})

(defn absolute
  "Positive absolute value of `concept-id`. Missing or non-positive amounts
  are nil."
  [concept-id]
  {:absolute concept-id})

(defn- ends
  [ds]
  (mapv #(str (:end %)) (ds/rows ds {:nil-missing? true})))

(defn- statement-by-item
  "Label → {end number} for the income lines the operating identity reads."
  [ds]
  (let [es (ends ds)]
    (into {}
          (keep (fn [id]
                  (let [label (:label (concepts/get-concept-id id))
                        by-end (into {}
                                     (keep (fn [[end v]]
                                             (when (number? v) [end v]))
                                           (map vector es (facts-ds/calc-concept ds id))))]
                    (when (seq by-end)
                      [label by-end]))))
          derived/statement-line-ids)))

(defn- at
  [by-item label end]
  (get (get by-item label) end))

(defn calc-operating-expenses
  "Operating expenses from the income-statement identity.

  revenue + non-operating income + asset gains − pre-tax income
  − cost of revenue − interest. Nil when revenue or pre-tax income is
  missing. Does not look at a reported operating-expenses tag; `derive` does."
  [ds]
  (let [by-item (statement-by-item ds)]
    (mapv (fn [end]
            (derived/derived-operating-expenses
             {:revenue (at by-item derived/revenue end)
              :pretax (at by-item derived/pretax-income end)
              :nonoperating-income (at by-item derived/nonoperating-income end)
              :asset-gains (at by-item derived/asset-gains end)
              :cost-of-revenue (at by-item derived/cost-of-revenue end)
              :interest (at by-item derived/interest-expense end)}))
          (ends ds))))

(defn calc-operating-income
  "Gross profit minus operating expenses, only where operating expenses came
  from the identity and operating income was not reported.

  Gross profit is the reported gross-profit line, else revenue − cost of
  revenue. Same periods as `derived/with-derived-lines`."
  [ds]
  (let [es (ends ds)
        by-item (statement-by-item ds)
        filled (derived/with-derived-lines by-item es)
        before (get by-item derived/operating-income)]
    (mapv (fn [end]
            (let [v (at filled derived/operating-income end)]
              (when (and (number? v)
                         (not (number? (get before end))))
                v)))
          es)))

(def ^:private calc-concept-ids
  {:operating-income-calc derived/statement-line-ids
   :operating-expenses-calc derived/statement-line-ids})

(def math
  {:operating-income-calc calc-operating-income
   :operating-expenses-calc calc-operating-expenses
   :operating-income (derive :operating-income :operating-income-calc)
   :operating-expenses (derive :operating-expenses :operating-expenses-calc)
   :dividends-paid (absolute :dividends-paid)
   :stock-repurchase (absolute :stock-repurchase)})

(defn math-concepts
  "Concept ids whose tags `kw` reads. Loaded onto the wide table before
  `calculate-math`."
  [kw]
  (let [entry (get math kw)]
    (cond
      (contains? calc-concept-ids kw) (vec (get calc-concept-ids kw))
      (:absolute entry) [(:absolute entry)]
      (:reported entry) (vec (distinct (concat [(:reported entry)]
                                               (math-concepts (:calc entry)))))
      :else [])))

(defn- absolute-vals
  [ds concept-id]
  (mapv (fn [v]
          (when (number? v)
            (let [amount (Math/abs (double v))]
              (when (pos? amount) amount))))
        (facts-ds/calc-concept ds concept-id)))

(defn calculate-math
  "Per-row values for `kw` in `math`, aligned with wide report-table `ds`."
  [ds kw]
  (let [entry (get math kw)]
    (cond
      (fn? entry) (entry ds)
      (:absolute entry) (absolute-vals ds (:absolute entry))
      (:reported entry)
      (mapv (fn [reported calc]
              (if (number? reported) reported calc))
            (facts-ds/calc-concept ds (:reported entry))
            (calculate-math ds (:calc entry)))
      :else (throw (ex-info "Unknown math key" {:kw kw})))))
