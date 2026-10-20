(ns demo.concept-calc
  "Demo: calc-concepts on an annual report-table for one ticker."
  (:require [edgar.api :as e]
            [edgar2.filing :as filing]
            [edgar2.financials.calc.concept :as calc]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.report.stats :as stats]))

(def ticker "AAPL")

(defn load-facts
  "Companyfacts for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (e/facts (filing/resolve-filer2 ticker)))

(defn annual-concepts
  "Annual report-table + `calc-concepts` for `ticker`."
  ([ticker]
   (annual-concepts ticker nil))
  ([ticker revenue-unit]
   (let [facts (load-facts ticker)
         table (facts-ds/report-table facts :y stats/stats-fields revenue-unit)]
     (calc/calc-concepts table))))

(defn run
  "Load demo ticker facts, print calc-concepts table."
  ([] (run ticker))
  ([ticker]
   (filing/ensure-identity!)
   (let [cc (annual-concepts ticker)]
     (println "\n=== calc-concepts" ticker "===")
     (println cc)
     cc)))

(defn -main
  [& args]
  (run (or (first args) ticker)))
