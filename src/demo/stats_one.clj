(ns demo.stats-one
  "Demo: stats-one for a single ticker via load-facts."
  (:require [clojure.pprint :as pprint]
            [edgar.api :as e]
            [edgar2.util :as util]
            [edgar2.util.core :as ucore]
            [edgar2.report.stats :as stats]))

(def ticker "AAPL")

(defn load-facts
  "Companyfacts dataset for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (e/facts (util/resolve-filer2 ticker)))

(defn- ticker-str [t]
  (cond (nil? t) ticker
        (clojure.core/string? t) t
        :else (name t)))

(defn run
  "Load facts for one ticker, run `stats/stats-one`, print the row.
  Accepts a ticker string, a symbol/keyword, or an exec-args map
  (e.g. `clj -X:stats-one :ticker '\"CPA\"'`)."
  ([] (run ticker))
  ([ticker-or-opts]
   (ucore/ensure-identity!)
   (let [ticker (if (map? ticker-or-opts)
                  (ticker-str (:ticker ticker-or-opts))
                  (ticker-str ticker-or-opts))
         cik (util/resolve-filer2 ticker)
         dss (load-facts ticker)
         st (stats/stats-one dss cik)]
     (println "\n=== stats-one" ticker "cik" cik "===")
     (pprint/pprint st)
     st)))
