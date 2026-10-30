(ns demo.report-table-full-shares
  "Print every `:shares-outstanding` tag, annual and quarterly.

  Usage: clj -X:demo-table-full-shares
         clj -X:demo-table-full-shares :ticker CCBG"
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.util :as util]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]))

(def fields
  "One column per tag on the `:shares-outstanding` concept, in concept order."
  (mapv (fn [[taxonomy concept]]
          {:field [taxonomy concept]
           :label (str (name taxonomy) ":" (name concept))
           :section :income})
        (:data (concepts/get-concept-id :shares-outstanding))))

(defn- ticker-name
  [ticker]
  (str/upper-case
   (cond
     (string? ticker) ticker
     (nil? ticker) "CCBG"
     :else (name ticker))))

(defn- print-table
  [title table]
  (println)
  (println title)
  (println (ds/print-all table))
  (doseq [{:keys [label]} fields]
    (println label (select-keys (meta (ds/column table label)) [:style :section]))))

(defn run
  "Load `:ticker` (default CCBG) and print every annual and quarterly row
  of the `:shares-outstanding` tags."
  ([] (run {}))
  ([{:keys [ticker]}]
   (util/ensure-identity!)
   (let [ticker (ticker-name ticker)
         dss (e/facts (util/resolve-filer2 ticker))]
     (print-table (str ticker " annual")
                  (facts-ds/report-table-full dss :y fields))
     (print-table (str ticker " quarterly")
                  (facts-ds/report-table-full dss :q fields))
     nil)))
