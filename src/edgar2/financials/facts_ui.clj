(ns edgar2.financials.facts-ui
  "Present a wide statement dataset for display.

  `statement` builds the period-row table from a view's field specs
  (`edgar2.financials.view.core/view-fields`). `format-columns`,
  `to-presentable`, and `remove-presentable-empty` prepare it for the
  financials page and CLI."
  (:require [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.financials.view.core :as view]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as col]))

(def ^:private period-col-names
  (set facts-ds/report-period-cols))

(def ^:private presentable-meta-cols
  #{:field :style :format :section})

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn- eps-column?
  [name]
  (boolean (re-find #"(?i)eps|per\s*share" (str name))))

(defn- scale-display
  "USD and share counts → millions. Per-share amounts stay in dollars."
  [name v]
  (when (number? v)
    (if (eps-column? name)
      (double v)
      (long (/ (double v) 1.0e6)))))

(defn- field-column-names
  [ds]
  (into []
        (remove #(contains? period-col-names %))
        (ds/column-names ds)))

(defn- make-column
  [name values metadata]
  (let [values (vec values)
        data (if (empty? values) (make-array Object 0) values)]
    (col/new-column name data metadata)))

(defn format-columns
  "Scale every field column on a wide statement dataset to millions.

  Leaves `:filed :period :end` alone. EPS / per-share columns stay in
  dollars. Column metadata is kept."
  [ds]
  (let [columns (mapv (fn [name]
                        (let [c (ds/column ds name)
                              metadata (dissoc (meta c) :name :datatype :n-elems)]
                          (if (contains? period-col-names name)
                            (make-column name (vec c) metadata)
                            (make-column name
                                         (mapv #(scale-display name %) c)
                                         metadata))))
                      (ds/column-names ds))]
    (ds/new-dataset {:dataset-name (or (ds/dataset-name ds) "report-table")}
                    columns)))

(defn to-presentable
  "Transpose a wide period-row statement into a field-row table.

  Period `:end` values become column names in row order (`:field` first).
  Field column order is unchanged. Adds `:style` (from column `:style` or
  `:format`), plus `:section` and `:format` when present. Keeps all-nil
  field rows."
  [ds]
  (let [rows (vec (ds/rows ds {:nil-missing? true}))
        ends (mapv #(str (:end %)) rows)
        field-names (field-column-names ds)
        present-rows
        (mapv (fn [name]
                (let [c (ds/column ds name)
                      metadata (meta c)
                      style (or (:style metadata) (:format metadata))
                      period-vals (mapv (fn [row] (as-number (get row name))) rows)]
                  (cond-> (into {:field (str name)}
                                (map vector ends period-vals))
                    style (assoc :style style)
                    (:format metadata) (assoc :format (:format metadata))
                    (:section metadata) (assoc :section (:section metadata)))))
              field-names)
        ;; ds/->dataset ignores :column-order for map rows; select-columns
        ;; is what actually pins :field then period ends in row order.
        meta-cols (filterv (fn [k]
                             (some #(contains? % k) present-rows))
                           [:style :format :section])
        col-order (into (into [:field] ends) meta-cols)
        table (ds/->dataset present-rows {:dataset-name "presentable"})]
    (if (seq present-rows)
      (ds/select-columns table col-order)
      table)))

(defn remove-presentable-empty
  "Drop presentable rows where every period value is non-numeric."
  [ds]
  (let [period-cols (into []
                          (remove #(contains? presentable-meta-cols %))
                          (ds/column-names ds))]
    (ds/filter ds
               (fn [row]
                 (some #(number? (get row %)) period-cols)))))

(defn presentable
  "Wide statement → scaled, transposed, empty rows removed."
  [ds]
  (-> ds format-columns to-presentable remove-presentable-empty))

(defn statement
  "Wide report-table for `view` over annual (`:y`) or quarterly (`:q`).

  Curated views use `report-table-full` with `view/view-fields`. `:all`
  uses `report-table` with no field filter (same orientation, no style
  metadata). Rows are newest `:end` first."
  [dss q-or-y view industry]
  (let [view (view/parse-view view)]
    (if (= view :all)
      (facts-ds/report-table dss q-or-y nil)
      (facts-ds/report-table-full dss q-or-y (view/view-fields view industry)))))
