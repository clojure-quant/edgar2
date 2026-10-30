(ns demo.stats-one
  "Demo: stats-one for a single ticker via load-facts."
  (:require [clojure.pprint :as pprint]
            [edgar.api :as e]
            [edgar2.util :as util]
            [edgar2.report.stats :as stats]))

(def ticker "AAPL")

(defn load-facts
  "Companyfacts dataset for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (e/facts (util/resolve-filer2 ticker)))

(defn run
  "Load facts for one ticker, run `stats/stats-one`, print the row."
  ([] (run ticker))
  ([ticker]
   (util/ensure-identity!)
   (let [cik (util/resolve-filer2 ticker)
         dss (load-facts ticker)
         st (stats/stats-one dss cik)]
     (println "\n=== stats-one" ticker "cik" cik "===")
     (pprint/pprint st)
     st)))

(defn -main
  [& args]
  (run (or (first args) ticker)))
