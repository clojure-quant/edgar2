(ns edgar2.financials.derived
  "Derived income-statement lines shared by the financials page and stats.

  Operating expenses, when that line is not already present:
  revenue + non-operating income + asset gains − pre-tax income
  − cost of revenue − interest.

  Operating profit, on those same periods, when operating income is not
  already present: gross profit − operating expenses.
  Gross profit is the gross-profit line, or revenue − cost of revenue."
  (:require [edgar2.financials.concepts :as concepts]))

(def operating-expenses (:label (concepts/get-concept-id :operating-expenses)))
(def operating-income (:label (concepts/get-concept-id :operating-income)))
(def gross-profit (:label (concepts/get-concept-id :gross-profit)))
(def revenue (:label (concepts/get-concept-id :revenue)))
(def cost-of-revenue (:label (concepts/get-concept-id :cost-of-revenue)))
(def pretax-income (:label (concepts/get-concept-id :pretax-income)))
(def nonoperating-income (:label (concepts/get-concept-id :nonoperating-income)))
(def asset-gains (:label (concepts/get-concept-id :asset-gains)))
(def interest-expense (:label (concepts/get-concept-id :interest-expense)))

(def statement-line-ids
  [:revenue :cost-of-revenue :gross-profit :operating-expenses
   :operating-income :nonoperating-income :asset-gains
   :interest-expense :pretax-income])

(def statement-lines
  "Line label → concept pairs, first match per line."
  (mapv (fn [id]
          (let [{:keys [label data]} (concepts/get-concept-id id)]
            [label data]))
        statement-line-ids))

(defn derived-operating-expenses
  "Operating expenses implied by the income-statement identity.
  Nil when revenue or pre-tax income is missing."
  [{:keys [revenue pretax nonoperating-income asset-gains cost-of-revenue interest]}]
  (when (and (number? revenue) (number? pretax))
    (- (+ (double revenue)
          (double (or nonoperating-income 0))
          (double (or asset-gains 0)))
       (double pretax)
       (double (or cost-of-revenue 0))
       (double (or interest 0)))))

(defn operating-profit
  "Gross profit minus operating expenses. Nil unless both are numbers."
  [gross-profit operating-expenses]
  (when (and (number? gross-profit) (number? operating-expenses))
    (- (double gross-profit) (double operating-expenses))))

(defn- at
  [by-item line end]
  (get (get by-item line) end))

(defn- gross-profit-at
  [by-item end]
  (or (at by-item gross-profit end)
      (let [rev (at by-item revenue end)
            cogs (at by-item cost-of-revenue end)]
        (when (and (number? rev) (number? cogs))
          (- (double rev) (double cogs))))))

(defn- fill-operating-expenses
  [by-item ends]
  (let [existing (or (get by-item operating-expenses) {})
        filled (reduce (fn [m end]
                         (if (number? (get m end))
                           m
                           (if-let [v (derived-operating-expenses
                                       {:revenue (at by-item revenue end)
                                        :pretax (at by-item pretax-income end)
                                        :nonoperating-income (at by-item nonoperating-income end)
                                        :asset-gains (at by-item asset-gains end)
                                        :cost-of-revenue (at by-item cost-of-revenue end)
                                        :interest (at by-item interest-expense end)})]
                             (assoc m end v)
                             m)))
                       existing
                       ends)]
    (assoc by-item operating-expenses filled)))

(defn- fill-operating-profit
  "Operating income on `derived-ends` only: periods whose operating
  expenses were just derived, and which do not already have operating income."
  [by-item derived-ends]
  (let [opex (or (get by-item operating-expenses) {})
        existing (or (get by-item operating-income) {})
        filled (reduce (fn [m end]
                         (if (number? (get m end))
                           m
                           (if-let [v (operating-profit (gross-profit-at by-item end)
                                                        (get opex end))]
                             (assoc m end v)
                             m)))
                       existing
                       derived-ends)]
    (assoc by-item operating-income filled)))

(defn with-derived-lines
  "`by-item` is {line-label {period-end number}}. Fills operating expenses
  on `ends` that lack them, then operating income on exactly those ends
  when operating income is absent. Reported numbers stay."
  [by-item ends]
  (let [before (or (get by-item operating-expenses) {})
        with-opex (fill-operating-expenses by-item ends)
        derived-ends (filter (fn [end]
                               (and (not (number? (get before end)))
                                    (number? (get-in with-opex [operating-expenses end]))))
                             ends)]
    (fill-operating-profit with-opex derived-ends)))
