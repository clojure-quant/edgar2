(ns demo.report-table-full
  "Print a mixed annual and quarterly report-table.

  `:show` is \"default\" or \"shares\". Default prints the income sample.
  Shares prints the three share-count tags.

  Usage: clj -X:demo-table-full
         clj -X:demo-table-full :ticker MSFT
         clj -X:demo-table-full :ticker MSFT :show shares
         clj -X:demo-table-full :ticker GP :show shares :ixbrl true"
  (:require [clojure.string :as str]
            [tech.v3.dataset :as ds]
            [edgar.api :as e]
            [edgar2.util :as util]
            [edgar2.util.core :as ucore]
            [edgar2.financials.facts-ds-full :as facts-ds-full] 
            ))

(def fields
  [{:field [:us-gaap :NetIncomeLoss]
    :label "NetIncome tag"
    :style :background-pl-green
    :section :income}
   {:field [:us-gaap :Assets]
    :label "Assets tag"
    :style :background-pl-green
    :section :income}
   #_{:concept :revenue
    :label "Revenue"
    :style :background-pl-green
    :section :income}
   #_{:edgarjure "Interest Expense"
    :label "Interest Expense"
    :style :background-pl-green
    :section :income}
   #_{:our :operating-income
    :label "Operating Income"
    :style :background-pl-green
    :section :income}])

(def share-fields
  [{:field [:dei :EntityCommonStockSharesOutstanding]
    :label "dei-Common"
    :section :income}
   {:field [:us-gaap :CommonStockSharesOutstanding]
    :label "gaap-common"
    :section :income}
   {:field [:us-gaap :WeightedAverageNumberOfSharesOutstandingBasic]
    :label "gaap-weigthed"
    :section :income}
   {:field [:us-gaap :WeightedAverageNumberOfSharesOutstandingBasicAndDiluted]
    :label "gaap-basic-diluted"
    :section :income}
   {:field [:us-gaap :WeightedAverageNumberOfDilutedSharesOutstanding]
    :label "gaap-diluted"
    :section :income}
   {:field [:ifrs-full :WeightedAverageShares]
    :label "ifrs-weighted"
    :section :income}
   {:field [:ifrs-full :NumberOfSharesOutstanding]
    :label "ifrs-number"
    :section :income}])

(defn- show-name
  [show]
  (str/lower-case
   (cond
     (nil? show) "default"
     (keyword? show) (name show)
     :else (str show))))

(defn- fields-for
  [show]
  (case (show-name show)
    "default" fields
    "shares" share-fields
    (throw (ex-info "show must be default or shares" {:show show}))))

(defn- ticker-name
  [ticker]
  (str/upper-case
   (cond
     (string? ticker) ticker
     (nil? ticker) "XOM"
     :else (name ticker))))

(defn- ixbrl?
  "True only for an explicit true. Absent or false stays on companyfacts."
  [ixbrl]
  (true? ixbrl))

(defn- print-table
  [title table shown]
  (println)
  (println title)
  (println (ds/print-all table))
  (doseq [{:keys [label]} shown]
    (println label (select-keys (meta (ds/column table label)) [:style :section]))))

(defn run
  "Load `:ticker` (default XOM) and print every annual and quarterly row.
  `:show` is \"default\" (income sample) or \"shares\" (the three share tags)."
  ([] (run {}))
  ([{:keys [ticker show]}]
   (ucore/ensure-identity!)
   (let [ticker (ticker-name ticker)
         shown (fields-for show)
         cik (util/resolve-filer2 ticker)
         dss  (e/facts cik)]
     (print-table (str ticker " annual")
                  (facts-ds-full/report-table-full dss :y shown)
                  shown)
     (print-table (str ticker " quarterly")
                  (facts-ds-full/report-table-full dss :q shown)
                  shown) 
     nil)))

(comment 
  (run {:ticker "MSFT"})

  (run {:ticker "SNPMF"})
  (util/resolve-filer2 "SNPMF")
  ;;=> "0001123658"

  ;
  )