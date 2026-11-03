(ns demo.regression
  "Table of log, asinh, and linear earnings fits for a few earnings vectors.

  Each method is shown twice, once with squared residuals and once with
  absolute residuals.

  Run with: clj -X:demo-regression"
  (:require [clojure.pprint :as pprint]
            [edgar2.regression.log :as log]
            [edgar2.financials.regression.linear :as linear]
            [edgar2.regression.asinh :as asinh]))

(def cases
  "Oldest-first annual earnings, labeled as supplied."
  [["ULCC" [251 -225 -102 -37 -11 85 -137]]
   ["RJET" [16 -182 -120 54 -91 64 76]]
   ["RJET (pre-COVID)" [33 47 27 16 -182 -120 54 -91 64 76]]
   ["MO" [-1293 4467 2475 5764 8130 11264 6947]]
   ["CPA" [247 -607 43 348 514 608 671]]])

(def ^:private regressions
  [["log" log/regression]
   ["asinh" asinh/regression]
   ["linear" linear/regression]])

(defn- fit-rows
  [vals]
  (for [[method fit] regressions
        error [:square :absolute]]
    (let [result (fit {:error error} vals)]
      {:method method
       :error error
       :stabilized-yoy (:earnings-stabilized-yoy result)
       :stabilized (:earnings-stabilized result)
       :variation (:earnings-variation result)})))

(defn- fit
  [label vals]
  (println (str "\n=== " label " " (pr-str vals) " ==="))
  (pprint/print-table [:method :error :stabilized-yoy :stabilized :variation]
                      (fit-rows vals)))

(defn run
  "Print a table of all squared and absolute residual fits for every case."
  ([] (run {}))
  ([_]
   (doseq [[label vals] cases]
     (fit label vals))))

(defn -main
  [& _]
  (run))

(comment 
  (run)
  ;
  )