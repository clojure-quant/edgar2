(ns edgar2.regression.db-test
  (:require [clojure.test :refer [deftest is]]
            [tech.v3.dataset :as ds]
            [edgar2.regression.db :as rdb]))

(defn- wide
  "Newest-first wide report-table-like dataset: one all-nil period at the
  newest end, then 10 populated annual periods carrying the tag columns
  `calc-concept` resolves."
  []
  (ds/->dataset
   (concat
    [{:filed "2025-08-01" :period "2025-FY" :end "2025-06-30"
      "Revenues" nil "NetIncomeLoss" nil}]
    (for [i (range 10)]
      (let [year (- 2024 i)]
        {:filed (str (inc year) "-02-15")
         :period (str year "-FY")
         :end (str year "-12-31")
         "Revenues" (+ 1000 (* i 100))
         "NetIncomeLoss" (+ 100 (* i 10))})))
   {:dataset-name "report-table"}))

(deftest model-dataset-drops-empty-periods-and-keeps-newest-ten
  (let [result (rdb/model-dataset (rdb/input-dataset (wide)) 10)]
    (is (= [:filed :period :end :revenue :net-income :row-index]
           (vec (ds/column-names result))))
    (is (= 10 (ds/row-count result)))
    (let [rows (vec (ds/rows result {:nil-missing? true}))
          newest (first rows)]
      (is (= "2024-12-31" (:end newest)))
      (is (= 1000 (:revenue newest)))
      (is (= 100 (:net-income newest)))
      (is (not-any? #(= "2025-06-30" (str (:end %))) rows)))))
