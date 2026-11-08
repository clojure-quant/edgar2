(ns edgar2.financials.facts-ds-full
  "Field-map wide reports built on `edgar2.financials.facts-ds`."
  (:require [edgar.api :as e]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as col]))

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn- concept-tag-pairs
  [id]
  (let [data (:data (concepts/get-concept-id id))]
    (when (sequential? data) data)))

(defn- jure-pairs
  [concept-names]
  (for [taxonomy [:us-gaap :ifrs-full :dei]
        concept concept-names]
    [taxonomy (keyword (str concept))]))

(defn- merge-chain
  "Standard (already in `acc`) first, then names only on this chain."
  [acc [label & names]]
  (let [names (vec names)
        cur (get acc label)]
    (assoc acc label
           (if (or (nil? cur) (= cur names))
             (or cur names)
             (vec (distinct (concat cur names)))))))

(defn- edgarjure-chain-index
  []
  (let [income (fn [industry]
                 (:chains (e/concepts-for :income :industry industry)))
        groups (concat [(income :standard)
                        (:chains (e/concepts-for :balance))]
                       (map income [:bank :insurance :reit]))]
    (reduce (fn [acc chains]
              (reduce merge-chain acc chains))
            {}
            groups)))

(def ^:private edgarjure-chains
  (delay (edgarjure-chain-index)))


(defn- field-tag-pairs
  "Taxonomy/concept pairs the wide table must contain for one field map."
  [field]
  (cond
    (:field field) [(:field field)]
    (:concept field) (concept-tag-pairs (:concept field))
    (:edgarjure field) (jure-pairs (get @edgarjure-chains (:edgarjure field)))
    :else []))

(defn- column-metadata
  [{:keys [style format section]}]
  (cond-> {}
    style (assoc :style style)
    format (assoc :format format)
    section (assoc :section section)))

(defn- make-column
  [name values metadata]
  (let [values (vec values)
        data (if (empty? values) (make-array Object 0) values)]
    (col/new-column name data metadata)))

(defn- column-vec
  [wide k]
  (if (ds/has-column? wide k)
    (mapv as-number (ds/column wide k))
    (vec (repeat (ds/row-count wide) nil))))

(defn- chain-vals
  [wide names]
  (let [names (mapv str names)]
    (mapv (fn [row]
            (some #(as-number (get row %)) names))
          (ds/rows wide {:nil-missing? true}))))

(defn- field-values
  [wide field]
  (cond
    (:field field) (column-vec wide (name (second (:field field))))
    (:concept field) (facts-ds/calc-concept wide (:concept field))
    (:edgarjure field) (chain-vals wide (get @edgarjure-chains (:edgarjure field)))
    :else (vec (repeat (ds/row-count wide) nil))))

(defn- period-column
  [wide k]
  (make-column k
               (if (ds/has-column? wide k) (vec (ds/column wide k)) [])
               {}))

(defn report-table-full
  "Wide report for field maps over annual (`:y`) or quarterly (`:q`) periods.

  Each map is one column, named by `:label`, newest `:end` first, with
  period columns `:filed :period :end`. `:style` and `:section` are column
  metadata.

  `:field` is one `[taxonomy concept]` pair. `:concept` is an id from
  `edgar2.financials.concepts`, via `calc-concept`. `:edgarjure` is a
  statement label resolved through `edgar.api/concepts-for`. "
  [dss q-or-y fields]
  (let [fields (vec fields)
        pairs (vec (distinct (mapcat field-tag-pairs fields)))
        wide (facts-ds/report-table dss q-or-y pairs)
        columns (into (mapv #(period-column wide %) facts-ds/report-period-cols)
                      (map (fn [field]
                             (make-column (:label field)
                                          (field-values wide field)
                                          (column-metadata field)))
                           fields))]
    (ds/new-dataset {:dataset-name "report-table"} columns)))
