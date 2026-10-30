(ns demo.concept
  "Demo: raw revenue facts, then calc-concept :revenue on annual and quarterly tables.

  Usage: clj -X:demo-concept :ticker SONY"
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.download.facts :as dl]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.util :as util]
            [edgar2.util.ticker-info :as ticker-info]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(defn ticker-name
  "Upper-case ticker, or nil when none was supplied."
  [ticker]
  (when (some? ticker)
    (let [s (str/upper-case
             (cond
               (string? ticker) ticker
               :else (name ticker)))]
      (when-not (str/blank? s) s))))

(defn print-usage
  []
  (println "Usage: clj -X:demo-concept :ticker SONY"))

(defn load-facts
  "Companyfacts for `ticker` (operating filer via resolve-filer2)."
  [ticker]
  (e/facts (util/resolve-filer2 ticker)))

(defn load-facts-zip
  "Companyfacts for `ticker` from data/companyfacts.zip.

  CIK comes from `resolve-filer2`, padded to 10 digits. The zip entry
  `CIK##########.json` is read with the same JSON mapper the other zip
  readers use, then `edgar.xbrl/facts->dataset`."
  [ticker]
  (let [cik (ticker-info/pad-cik (util/resolve-filer2 ticker))
        zip-path (dl/ensure-zip! {})]
    (when-not cik
      (throw (ex-info "no CIK for ticker" {:ticker ticker})))
    (with-open [zf (ZipFile. (str zip-path))]
      (let [name (str "CIK" cik ".json")
            entry (.getEntry zf name)]
        (when-not entry
          (throw (ex-info "no companyfacts entry in zip"
                          {:ticker ticker :cik cik :entry name :zip zip-path})))
        (xbrl/facts->dataset
         (with-open [in (.getInputStream zf entry)]
           (json/read-value in json/keyword-keys-object-mapper)))))))

(def raw-fact-cols
  "Observation fields from companyfacts, in docs/data-facts.md order."
  [:form :filed :accn
   :start :end :fy :fp  :frame
   :concept :val])

(def raw-concepts
  ["Revenue"
   "RevenueFromContractWithCustomerExcludingAssessedTax"
   "Revenues"])

(defn- raw-cols
  "Observation columns, then revenue-tag columns, skipping any that `facts` lacks."
  [facts]
  (let [present? #(ds/has-column? facts %)]
    (into (filterv present? raw-fact-cols)
          (filter present? raw-concepts))))

(defn- cap-concept
  "`:concept` no longer than 30 characters."
  [s]
  (when (some? s)
    (let [s (str s)]
      (if (> (count s) 30) (subs s 0 30) s))))

(defn raw-table-recent
  "Last 50 companyfacts rows, oldest `:filed` first. Same columns as
  `raw-table`, with no `:concept` filter. `:concept` is capped at 30
  characters."
  [facts]
  (let [table (-> facts
                  (ds/set-dataset-name "raw-recent")
                  (ds/sort-by-column :filed compare)
                  (ds/select-columns (raw-cols facts))
                  (ds/tail 50))]
    (if (ds/has-column? table :concept)
      (ds/add-or-update-column table :concept
                               (mapv cap-concept (ds/column table :concept)))
      table)))

(defn raw-table
  "Companyfacts rows whose `:concept` is one of `raw-concepts`, oldest
  `:filed` first, last 50 only. Columns match `raw-table-recent`."
  [facts]
  (-> facts
      (ds/set-dataset-name "raw")
      (ds/filter-column :concept (set raw-concepts))
      (ds/sort-by-column :filed compare)
      (ds/select-columns (raw-cols facts))
      (ds/tail 50)))

(defn with-revenue
  "Annual report-table for all :revenue tags, plus :revenue-c from calc-concept."
  [facts y-or-q]
  (let [fields (:data (concepts/get-concept-id :revenue))
        table (facts-ds/report-table facts y-or-q fields)
        revenue-c (facts-ds/calc-concept table :revenue)]
    (ds/add-or-update-column table :revenue-c revenue-c)))

(defn run
  "Print the raw revenue facts for `:ticker`, then annual and quarterly
  tables with :revenue-c. With no ticker, print usage.

  Usage: clj -X:demo-concept :ticker SONY"
  ([] (run {}))
  ([{:keys [ticker]}]
   (if-let [ticker (ticker-name ticker)]
     (do
       (util/ensure-identity!)
       (let [f (load-facts-zip ticker)
             table (with-revenue f :y)
             table-q (with-revenue f :q)]
         (println "\n=== raw-recent " ticker "===")
         (println (ds/print-all (raw-table-recent f)))
         (println "\n=== raw " ticker "===")
         (println (ds/print-all (raw-table f)))
         (println "\n=== y " ticker "===")
         (println table)
         (println "\n=== q " ticker "===")
         (println table-q)
         nil))
     (print-usage))))
