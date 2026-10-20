(ns demo.universe-cur
  "Demo: universe-row-by-cik for USD / CAD / GBP filers."
  (:require [clojure.pprint :as pprint]
            [edgar2.db.universe :as universe]))

;; One stock per reporting currency from data/universe-facts.edn.
(def samples
  [{:label "USD" :ticker "AAPL" :cik "0000320193" :name "Apple Inc."}
   {:label "CAD" :ticker "CNI" :cik "0000016868" :name "Canadian National Railway Co"}
   {:label "GBP" :ticker "BCS" :cik "0000312069" :name "Barclays PLC"}])

(defn run
  "Print universe-facts rows for one USD, one CAD, and one GBP stock."
  []
  (doseq [{:keys [label ticker cik name]} samples]
    (let [row (universe/universe-row-by-cik cik)]
      (println (format "\n=== %s — %s (%s) cik %s ===" label ticker name cik))
      (pprint/pprint row)))
  nil)

(defn -main
  [& _]
  (run))
