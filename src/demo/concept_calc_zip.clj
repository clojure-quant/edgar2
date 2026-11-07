(ns demo.concept-calc-zip
  "Demo: calc-concepts on an annual report-table for one ticker."
  (:require
   [tech.v3.dataset :as ds]
   [edgar.api :as e]
   [edgar2.util :as util]
   [edgar2.util.core :as ucore]
   [edgar2.financials.calc.concept :as calc]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.financials.calc.current :as annual]
   [edgar2.financials.facts-zip :as z]
   [edgar2.db.ticker :as t]
   [tablecloth.api :as tc]))

(def fz (z/open-facts-zip))

(defn load-facts-zip [ticker]
  (z/load-dataset fz (t/ticker->cik ticker)))

(defn load-facts
  "Companyfacts for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (e/facts (util/resolve-filer2 ticker)))

(defn annual-concepts
  "Annual report-table + `calc-concepts` for `ticker`."
  [ticker]
  (let [facts (load-facts-zip ticker)
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
  
  (-> (load-facts-zip "MO")
      (facts-ds/annual-periods))
  
 ; 
  )
