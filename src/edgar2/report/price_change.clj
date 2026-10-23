(ns edgar2.report.price-change
  "Close lookbacks for the EODHD universe.

  Runs the price-change screen and writes one map per name.

  Usage: clj -X:calculate-price-change"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.algo.price-change :as price-change]
            [edgar2.screen.core :as screen]))

(def path "data/price-change.edn")

(def field-order
  [:ticker :close :close50 :close200 :close400
   :change50 :change200 :change400])

(defn- ordered
  "Map with `field-order` keys, including nil lookbacks."
  [row]
  (apply array-map
         (mapcat (fn [k] [k (get row k)]) field-order)))

(defn save!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str path ".tmp")
        ordered-rows (mapv ordered (sort-by #(or (:ticker %) "") rows))]
    (spit tmp (with-out-str (pprint/pprint ordered-rows)))
    (.renameTo (io/file tmp) (io/file path))))

(defn run
  "Write data/price-change.edn.

  Usage: clj -X:calculate-price-change"
  [_]
  (let [result (screen/run! price-change/screen {})
        rows (:rows result)]
    (save! rows)
    (doseq [line (:lines result)]
      (println line))
    (println (screen/status-line result))
    (println (format "Wrote %s  (%d names; close50=%d  close200=%d  close400=%d)"
                     path
                     (count rows)
                     (count (filter :close50 rows))
                     (count (filter :close200 rows))
                     (count (filter :close400 rows))))
    nil))
