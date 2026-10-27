(ns edgar2.financials.calc.quarterly
  "Quarterly concept vectors and period-over-period changes."
  (:require [edgar2.financials.facts-ds :as facts-ds]))

(defn shift-4
  "Newest-first vector shifted by 4 rows (earlier periods move up).

  Index 0 of the result is the value that was at index 4. The last four
  slots are nil. When fewer than 4 values exist, returns a same-length
  vector of nils."
  [vals]
  (let [v (vec vals)
        n (count v)]
    (if (< n 4)
      (vec (repeat n nil))
      (into (subvec v 4) (repeat 4 nil)))))

(defn abs-change-pct
  "Percent change with |prior| as denominator (Damodaran), so loss narrowing
  is positive and loss widening is negative."
  [prev cur]
  (when (and (number? prev) (number? cur) (not (zero? (double prev))))
    (* 100.0 (/ (- (double cur) (double prev))
                (Math/abs (double prev))))))

(defn- change-q-on-y
  "Newest numeric percent change of `concept` vs the value 4 rows later.

  `ds-quarterly` is a quarterly report-table. Uses |prior| in the
  denominator so a shrinking loss stays positive."
  [ds-quarterly concept]
  (let [xs (facts-ds/calc-concept ds-quarterly concept)
        prior (shift-4 xs)]
    (->> (map abs-change-pct prior xs)
         (some #(when (number? %) %)))))

(defn net-income-change-q-on-y
  "Most recent quarter net income vs net income 4 quarters earlier (%).

  `ds-quarterly` is a quarterly report-table. Net income from
  `facts-ds/calc-concept` `:net-income` (row-aligned; same currency assumed).
  Compares each period to the value 4 rows later via `shift-4`, then takes
  the newest pair where both sides are numeric. Uses |prior| in the
  denominator so negative earnings keep the right sign."
  [ds-quarterly]
  (change-q-on-y ds-quarterly :net-income))

(defn sales-change-q-on-y
  "Most recent quarter revenue vs revenue 4 quarters earlier (%).

  Same comparison as `net-income-change-q-on-y`, on `:revenue`."
  [ds-quarterly]
  (change-q-on-y ds-quarterly :revenue))
