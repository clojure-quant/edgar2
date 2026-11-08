(ns demo.concept-calc-zip
  "Demo: calc-concepts on an annual report-table for one ticker."
  (:require
   [tech.v3.dataset :as ds]
   [edgar2.util :as util]
   [edgar2.util.core :as ucore]
   [edgar2.financials.load.core :refer [load-facts]]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.financials.calc.concept :as calc]
   [edgar2.financials.calc.current :as annual]
   [edgar2.db.ticker :as t]
   [tablecloth.api :as tc]))

(defn load-facts-ticker
  "Companyfacts for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (load-facts (util/resolve-filer2 ticker)))

(defn annual-concepts
  "Annual report-table + `calc-concepts` for `ticker`."
  [ticker]
  (let [facts (load-facts-ticker ticker)
        table (facts-ds/report-table facts :y annual/annual-fields)]
    (calc/calc-concepts table)))

(defn run
  "Load demo ticker facts, print calc-concepts table."
  ([{:keys [ticker]}]
   (ucore/ensure-identity!)
   (let [cc (annual-concepts ticker)]
     (println "\n=== calc-concepts" ticker "===")
     (println cc)
     cc)))

(comment
  (->
   (run {:ticker "MO"})
   (ds/select-columns [:filed :period :end :revenue]))

  (-> (load-facts-ticker "MO")
      (facts-ds/annual-periods))

 ; 
  )
