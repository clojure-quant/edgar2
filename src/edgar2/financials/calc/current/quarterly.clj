(ns edgar2.financials.calc.current.quarterly
  "Quarterly concept vectors and period-over-period changes."
  (:require [edgar2.financials.facts-ds :as facts-ds]
            [tablecloth.api :as tc]))

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
  (let [xs (get ds-quarterly concept)
        prior (shift-4 xs)]
    (->> (map abs-change-pct prior xs)
         (some #(when (number? %) %)))))


(defn calculate-quarterly [q-ds]
  #_(println "quarterly summary\r\n"
           (-> q-ds
               (tc/select-columns [:filed :period :start :end :revenue :net-income])
               (tc/select-rows (range 10))) ) 
  {:sales-change-q-on-y (change-q-on-y q-ds :revenue)
   :net-income-change-q-on-y (change-q-on-y q-ds :net-income)})
