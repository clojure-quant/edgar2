(ns demo.earnings-regression
  "Checks of the earnings regression.

  PAY: 2022 is a loss. It stays in the vector so its slot still counts as a
  year, and the fit drops it.

  SW: annual net income from /company/financials/10. That page asks for 10
  years; Smurfit Westrock has four annual columns, 2022 through 2025."
  (:require [clojure.pprint :as pprint]
            [edgar2.financials.calc.regression :as regression]))

(def pay-earnings
  "Oldest-first annual net income for PAY, including the 2022 loss."
  [9300000 -513000 22300000 44200000 66900000])

(def sw-earnings
  "Oldest-first annual net income for SW, in dollars.

  /company/financials/10 shows these as $ millions: 1034, 825, 319, 699."
  [1034000000 825000000 319000000 699000000])

(defn- fit
  [label vals]
  (let [result (regression/earnings-regression vals)]
    (println label)
    (pprint/pprint result)
    result))

(defn run
  "Fit PAY, then the SW earnings vector, and print both."
  []
  (fit "PAY" pay-earnings)
  (fit "SW" sw-earnings))


(defn -main
  [& _]
  (run))
