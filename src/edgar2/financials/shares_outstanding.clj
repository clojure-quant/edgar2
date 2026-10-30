(ns edgar2.financials.shares-outstanding
  "Shares outstanding from annual and quarterly report tables.

  `report-table-full` is run for `:y` and `:q` over every tag on the
  `:shares-outstanding` concept. `calc-concept` then adds a
  `:shares-outstanding` column to both datasets."
  (:require [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as col]))

(defn- make-column
  [name values metadata]
  (let [values (vec values)
        data (if (empty? values) (make-array Object 0) values)]
    (col/new-column name data metadata)))

(defn shares-outstanding-fields
  "One `:field` map per tag on the `:shares-outstanding` concept.

  `:label` is the concept local name, which is the column `calc-concept`
  reads."
  []
  (mapv (fn [[taxonomy concept]]
          {:field [taxonomy concept]
           :label (name concept)})
        (or (:data (concepts/get-concept-id :shares-outstanding)) [])))

(defn with-shares-outstanding
  "Copy of a wide report table with a `:shares-outstanding` column.

  The column is `calc-concept` for `:shares-outstanding`, one value per row,
  in the same order as `table`."
  [table]
  (let [values (vec (facts-ds/calc-concept table :shares-outstanding))
        columns (conj (mapv #(ds/column table %) (ds/column-names table))
                      (make-column :shares-outstanding values {}))]
    (ds/new-dataset {:dataset-name (or (ds/dataset-name table) "report-table")}
                    columns)))

(defn- report-or-nil
  "Nil when `q-or-y` has no periods."
  [dss q-or-y fields]
  (try
    (facts-ds/report-table-full dss q-or-y fields)
    (catch Exception _
      nil)))

(defn shares-outstanding-tables
  "Annual and quarterly report tables for every `:shares-outstanding` tag.

  Keys are `:y` and `:q`. Each table is `report-table-full` over
  `shares-outstanding-fields`, plus a `:shares-outstanding` column from
  `calc-concept`. A frequency with no periods is nil."
  [dss]
  (let [fields (shares-outstanding-fields)]
    (into {}
          (map (fn [q-or-y]
                 [q-or-y (when dss
                           (some-> (report-or-nil dss q-or-y fields)
                                   with-shares-outstanding))]))
          [:y :q])))

(defn shares-table-from-selected
  "Wide `:shares-outstanding` table from a long `select-data` dataset.

  Same columns as `shares-outstanding-tables`: one per concept tag, plus
  `:shares-outstanding` from `calc-concept`. Nil when `selected` is empty."
  [selected]
  (when (and selected (pos? (ds/row-count selected)))
    (-> (facts-ds/report-table-selected selected (mapv :field (shares-outstanding-fields)))
        with-shares-outstanding)))

(defn latest-shares-row
  "Newest row of `table` whose `:shares-outstanding` is a positive number.

  Tables from `report-table-full` are newest `:end` first."
  [table]
  (when (and table
             (pos? (ds/row-count table))
             (ds/has-column? table :shares-outstanding))
    (some (fn [row]
            (let [v (:shares-outstanding row)]
              (when (and (number? v) (pos? (double v)))
                {:val v
                 :end (some-> (:end row) str not-empty)
                 :filed (some-> (:filed row) str not-empty)
                 :period (some-> (:period row) str not-empty)})))
          (ds/rows table {:nil-missing? true}))))

(defn choose-shares-outstanding
  "Positive `:shares-outstanding` from `q` when there is no annual row,
  or when the quarterly row was filed after `y`.

  With no annual financial statements, the latest quarterly count is the
  share count. A quarterly count more than 100 times smaller than the
  annual count is left aside. Those rows are a share count reported
  without the thousands multiplier. `y` and `q` are wide tables that
  already have a `:shares-outstanding` column. Returns the chosen row
  (`:val`, `:end`, `:filed`, `:period`) with `:q-or-y` set to `:y` or `:q`."
  [y q]
  (let [quarterly (latest-shares-row q)
        annual (latest-shares-row y)
        quarterly-filed-after? (and quarterly annual
                                    (pos? (compare (str (:filed quarterly))
                                                   (str (:filed annual)))))
        quarterly-too-small? (and quarterly annual
                                 (> (/ (double (:val annual))
                                       (double (:val quarterly)))
                                    100.0))
        [q-or-y chosen] (cond
                          (and quarterly (nil? annual))
                          [:q quarterly]
                          (and quarterly
                               quarterly-filed-after?
                               (not quarterly-too-small?))
                          [:q quarterly]
                          annual [:y annual])]
    (when chosen
      (assoc chosen :q-or-y q-or-y))))

(defn shares-outstanding-obs
  "Share count from `shares-outstanding-tables`.

  When the company has no annual statements, this is the latest quarterly
  `:shares-outstanding`. Otherwise the quarterly row is used when it was
  filed after the annual row, unless it is more than 100 times smaller.
  The value is that row's `calc-concept` of `:shares-outstanding`."
  [dss]
  (let [{:keys [y q]} (shares-outstanding-tables dss)]
    (choose-shares-outstanding y q)))
