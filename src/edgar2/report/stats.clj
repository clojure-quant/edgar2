(ns edgar2.report.stats
  "Universe fundamentals from companyfacts.zip: sales growth, margin, ROC."
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [edgar2.download.facts :as dl]
   [edgar2.util.core :as ucore]
   [edgar2.db.universe :as db-universe]
   [edgar2.report.stats.core :as stats]
   [edgar2.financials.facts-zip :as facts-zip]
   [edgar2.util.map-progress :refer [map-progress]]))

(def stats-path "data/stats.edn")

(defn save-stats!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str stats-path ".tmp")
        data (into [] rows)]
    (spit tmp (with-out-str (pprint/pprint data)))
    (.renameTo (io/file tmp) (io/file stats-path))))


(defn- stats-progress-line
  [i total st]
  (format "  %d/%d  %s  %s" i total (:ticker st) (:cik st)))


(defn stats
  "Usage: clj -X:stats
         clj -X:stats :limit 25"
  ([] (stats {}))
  ([{:keys [limit force]
     :or {force false}}]
   (ucore/ensure-identity!)
   (let [zip-path (dl/ensure-zip! {:force force})
         all (db-universe/load-universe)
         filers (vec (cond->> all limit (take (long limit))))
         total (count filers)]
     (println (format "stats  %d of %d universe  ← %s" total (count all) zip-path))
     (flush)
     (with-open [zf (facts-zip/open-facts-zip)]
       (let [out (map-progress
                  (fn [i total st]
                    (stats-progress-line i total st))
                  (fn [row]
                    (let [cik (:cik row)
                          dss (try (facts-zip/load-dataset zf cik)
                                   (catch Exception ex
                                     (println (format "  skip %s (%s)" cik (.getMessage ex)))
                                     nil))
                          st (try 
                               (stats/stats-cik cik)
                               (catch Exception ex 
                                 (println (format "cik: %s calculate ex: %s" cik (ex-message ex)))
                                 nil))
                          st (when st (stats/clean st))]
                      st))
                  filers)
             good (remove nil? out)
             ]
         (println "stats calc complete!")
         (flush)
         (save-stats! good)))
     nil)))

(comment 
  (stats {:limit 300})
  (stats {})
  (stats {:limit 10})
  
  ;
  )