(ns edgar2.download.xbrl-shares
  "Share counts from a companyfacts file, or from the latest annual filing.

  clj -X:xbrl-shares
  clj -X:xbrl-shares :ticker '\"SONY\"'
  clj -X:xbrl-shares :file '\"test/facts/COKE.json\"'"
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.db.universe :as universe]
            [edgar2.download.dataset :as dataset]
            ;[edgar2.filing.xbrl.xbrl-shares-v1 :as shares-v1]
            [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
            [edgar2.util :as util]
            [edgar2.util.core :as ucore]
            [edgar2.util.map-progress :refer [map-progress]])
  (:import [java.time LocalDate]))


(defn share-summary-ticker
  "CIK for `ticker`, then `share-summary-cik`, with `:ticker` added."
  [ticker]
  (ucore/ensure-identity!)
  (ucore/enable-cache)
  (let [cik (util/resolve-filer2 ticker)]
    (if cik
      (xbrl-shares/share-summary-ticker ticker)
      {:ticker ticker :error "no cik found for ticker"})))

(def share-classes-path
  "Written by `run` when no ticker is passed."
  "data/share-classes.edn")

(defn- summary-for-universe-row
  "Share summary for one universe-facts row. A failure is a row with :error."
  [{:keys [ticker cik]}]
  (try
    (assoc (share-summary-ticker ticker) :ticker ticker)
    (catch Throwable ex
      (let [msg (or (ex-message ex) (str ex))]
        (println (format "  skip %s %s (%s)" ticker cik msg))
        (flush)
        {:ticker ticker :cik cik :error msg}))))

(defn run-universe
  "Share summary for every ticker in data/universe-facts.edn.

  Progress prints every 100 tickers. The file is written after the last one."
  [limit]
  (ucore/ensure-identity!)
  (ucore/enable-cache)
  (let [rows (->> (universe/load-universe)
                  (filter #(not (str/blank? (str (:ticker %)))))
                  vec)
        rows (if limit (take limit rows) rows)
        n (count rows)]
    (println (format "share classes  %d tickers  → %s" n share-classes-path))
    (flush)
    (let [out (map-progress
               (fn [i total result]
                 (format "  %d/%d  %s" i total (:ticker result)))
               summary-for-universe-row
               rows)]
      (dataset/write-dataset! share-classes-path
                              {:download-date (str (LocalDate/now))}
                              out)
      (println (format "Wrote %s  (%d tickers)" share-classes-path (count out)))
      out)))

(defn run
  "With `:ticker`, print `share-summary-ticker`. 
   summarize every universe ticker into  data/share-classes.edn."
  ([] (run {}))
  ([{:keys [ticker limit]}]
   (if ticker
     ; single ticker
     (let [summary (share-summary-ticker (name ticker))]
       (pprint/pprint summary)
       summary)
     ; universe
     (let [limit (when limit
                   (if (string? limit)
                     (parse-long limit) limit))]
       (run-universe limit)))))


(comment

  (run {:ticker "MSFT"})
  (run {:ticker "AIR"})

  (run {:limit "5"})
  (run {:limit 5})
  
 ; 
  )