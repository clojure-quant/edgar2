(ns demo.earnings-regression
  "PAY check of the earnings regression.

  2022 is a loss. It stays in the vector so its slot still counts as a year,
  and the fit drops it."
  (:require [clojure.pprint :as pprint]
            [edgar2.financials.calc.regression :as regression]))

(def pay-earnings
  "Oldest-first annual net income for PAY, including the 2022 loss."
  [9300000 -513000 22300000 44200000 66900000])

(defn run
  "Fit `pay-earnings` and print the two result keys."
  []
  (let [result (regression/earnings-regression pay-earnings)]
    (pprint/pprint result)
    result))

(defn -main
  [& _]
  (run))
