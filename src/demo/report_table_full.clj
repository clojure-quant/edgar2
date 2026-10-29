(ns demo.report-table-full
  "Print a mixed annual and quarterly report-table.

  Usage: clj -X:demo-table-full
         clj -X:demo-table-full :ticker MSFT"
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.filing :as filing]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]))

(def fields
  [{:field [:us-gaap :Revenues]
    :label "Revenue tag"
    :style :background-pl-green
    :section :income}
   {:concept :revenue
    :label "Revenue"
    :style :background-pl-green
    :section :income}
   {:edgarjure "Interest Expense"
    :label "Interest Expense"
    :style :background-pl-green
    :section :income}
   {:math :operating-income
    :label "Operating Income"
    :style :background-pl-green
    :section :income}])

(defn- ticker-name
  [ticker]
  (str/upper-case
   (cond
     (string? ticker) ticker
     (nil? ticker) "XOM"
     :else (name ticker))))

(defn- print-table
  [title table]
  (println)
  (println title)
  (println (ds/print-all table))
  (doseq [{:keys [label]} fields]
    (println label (select-keys (meta (ds/column table label)) [:style :section]))))

(defn run
  "Load `:ticker` (default XOM) and print every annual and quarterly row."
  ([] (run {}))
  ([{:keys [ticker]}]
   (filing/ensure-identity!)
   (let [ticker (ticker-name ticker)
         dss (e/facts (filing/resolve-filer2 ticker))]
     (print-table (str ticker " annual")
                  (facts-ds/report-table-full dss :y fields))
     (print-table (str ticker " quarterly")
                  (facts-ds/report-table-full dss :q fields))
     nil)))
