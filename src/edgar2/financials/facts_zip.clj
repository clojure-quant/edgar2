(ns edgar2.financials.facts-zip
  "One companyfacts JSON from data/companyfacts.zip."
  (:require [edgar.xbrl :as xbrl]
            [edgar2.download.facts :as dl]
            [edgar2.util.zip :as zip]
            [jsonista.core :as json])
  (:import [java.util.zip ZipFile]))

(def ^:private cik-entry-re
  #"CIK(\d+)\.json")

(defn read-cik-json
  "Companyfacts map for `cik` in open zip `zf`, or nil when the entry is absent."
  [zf cik]
  (when cik
    (let [entry (.getEntry zf (str "CIK" cik ".json"))]
      (when entry
        (with-open [in (.getInputStream zf entry)]
          (json/read-value in json/keyword-keys-object-mapper))))))

(defn cik-list
  "10-digit CIKs for companyfacts entries in open zip `zf`."
  [zf]
  (->> (zip/get-file-info zf)
       (keep #(second (re-matches cik-entry-re %)))
       vec))

(defn open-facts-zip
  "Open data/companyfacts.zip. Caller closes the ZipFile with with-open."
  []
  (ZipFile. (str dl/companyfacts-zip-path)))

(defn load-dataset
  "Companyfacts dataset for `cik` in open zip `zf`, or nil when the entry
  is missing or does not parse."
  [zf cik]
  (when-let [data (read-cik-json zf cik)]
    (try
      (xbrl/facts->dataset data)
      (catch Exception ex
        (println (format "  facts-ds %s (%s)" cik (.getMessage ex)))
        nil))))
