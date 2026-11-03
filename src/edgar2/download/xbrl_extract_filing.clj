(ns edgar2.download.xbrl-extract-filing
  "Companyfacts-shaped map from a filing cover and its statement exhibit.

  clj -X:xbrl-extract-filing
  clj -X:xbrl-extract-filing :url '\"http://localhost:3000/filing/0000317540/0001628280-26-009057/coke-20251231.htm\"'
  clj -X:xbrl-extract-filing :file '\"/path/to/filing.htm\"' :out '\"/tmp/coke-facts.json\"'"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.filing.xbrl-facts :as xbrl-facts]
            [edgar2.filing.xbrl-loader :as xbrl-loader]
            [jsonista.core :as json]))
(def ^:private json-mapper
  (json/object-mapper
   {:pretty true
    :encode-key-fn (fn [k]
                     (if (keyword? k) (name k) (str k)))}))

(defn- accession
  [s]
  (second (re-find #"(\d{10}-\d{2}-\d{6})" (str s))))


(defn- taxonomy-counts
  [facts]
  (mapv (fn [[taxonomy concepts]]
          {:taxonomy (name taxonomy)
           :concepts (count concepts)
           :facts (reduce + 0
                          (for [[_ details] concepts
                                [_ rows] (:units details)]
                            (count rows)))})
        facts))

(defn- print-usage
  []
  (println "Usage: clj -X:xbrl-extract-filing :url '\"http://localhost:3000/filing/...htm\"'")
  (println "       clj -X:xbrl-extract-filing :file '\"filing.htm\"' :out '\"facts.json\"'"))

(defn run
  "Parse a filing cover and its statement exhibit, when linked.
  Prints the companyfacts header, fact counts, and
  dei:EntityCommonStockSharesOutstanding. With `:out`, write the full map
  as JSON."
  ([] (run {}))
  ([{:keys [out] :as args}]
   (if-let [docs (xbrl-loader/load-documents (or (:file args) (:url args)))]
     (let [where (-> docs first :where)
          accn (accession where)
           parsed (xbrl-facts/parse-all (map :text docs)
                                  (cond-> {} accn (assoc :accn accn)))
           shares (get-in parsed [:facts :dei :EntityCommonStockSharesOutstanding])]
       (println (:entityName parsed) " cik " (:cik parsed))
       (doseq [{:keys [where]} docs]
         (println "source " where))
       (when accn
         (println "accn   " accn))
       (doseq [{:keys [taxonomy concepts facts]} (taxonomy-counts (:facts parsed))]
         (println (format "%-12s concepts %4d   facts %4d" taxonomy concepts facts)))
       (println)
       (println "dei:EntityCommonStockSharesOutstanding")
       (pprint/pprint shares)
       (when (and out (not (str/blank? (str out))))
         (spit (io/file (str out))
               (json/write-value-as-string parsed json-mapper))
         (println "wrote  " out))
       nil)
     (print-usage))))
