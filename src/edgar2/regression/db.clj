(ns edgar2.regression.db
  (:require
   [tech.v3.dataset :as ds]
   [edgar2.financials.facts-zip :as z]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.regression.net-income-model :as nim]))

(def fz (z/open-facts-zip))

(defn input-dataset [wide]
  (-> (ds/select-columns wide [:filed :period :end])
      (ds/add-or-update-column
       :revenue (facts-ds/calc-concept wide :revenue))
      (ds/add-or-update-column
       :net-income (facts-ds/calc-concept wide :net-income))
      (ds/add-or-update-column
       :row-index (range (ds/row-count wide)))))

(defn model-dataset
  "Newest-first dataset of `:filed :period :end :revenue :net-income`.

  Adds `:revenue` and `:net-income` via `calc-concept` to a wide annual
  report-table, drops period-ends where both are non-numeric, and keeps the
  newest 10 rows (report-table is newest-`:end`-first)."
  [input-ds n-max]
  (-> input-ds
      (ds/filter #(or (number? (:revenue %))
                      (number? (:net-income %))))
      (ds/head n-max)))

(defn add-estimate-existing [{:keys [p rows-ni all-ds]}]
  (let [row-idx-est-dict (->> (map vector rows-ni p)
                              (into {}))
        get-est (fn [idx]
                  (get row-idx-est-dict idx))]
    (ds/add-or-update-column all-ds :estimate (mapv get-est (ds/column all-ds :row-index)))))

(defn future-estimates-ds
  [row]
  (let [estimates (nim/predict-future row 3)]
    (ds/->dataset {:period   (repeat (count estimates) "future")
                   :end      (mapv #(str %) (range (count estimates) 0 -1))
                   :estimate (map double estimates)}
                  {:dataset-name "future-estimates"})))




(defn combined-ds [row]
  (ds/concat
   (future-estimates-ds (:model row))
   (add-estimate-existing row)))

(defn ni-model-calculate [wide]
  (try
    (let [all-ds (input-dataset wide)
          model-ds (model-dataset all-ds 5)
          ni (->> model-ds
                  :net-income
                  (into [])
                  reverse
                  (into []))
          rows-ni (->> model-ds
                       :row-index
                       (into [])
                       reverse
                       (into []))
          m  (nim/classify-series ni)
          p (nim/predict-past m)
          row {:all-ds all-ds
               :model-ds model-ds
               :ni ni
               :model m
               :p p
               :rows-ni rows-ni
               :earnings-stabilized (nim/predict-current m)
               :earnings-stabilized-yoy (* 100 (:stabilized-yoy m))
               :earnings-variation (nim/certainity m)
               }]
      (assoc row :combined-ds (combined-ds row)))
    (catch Exception ex {:model-error (ex-message ex)})))

(defn ni-model [cik]
  (try
    (let [raw (z/load-dataset fz cik)
          wide (facts-ds/report-table raw :y nil)]
      (assoc 
       (ni-model-calculate wide)
       :cik cik))
    (catch Exception _ nil)))

(defn ni-model-facts [facts]
  (let [y (facts-ds/report-table facts :y nil)]
    (ni-model-calculate y)))


(comment
  (require '[edgar2.db.ticker :as t])
  (-> (ni-model (t/ticker->cik "MSFT"))
      :model)
  (-> (ni-model (t/ticker->cik "MSFT"))
      :model-ds)
  (-> (ni-model (t/ticker->cik "MSFT"))
      :p)
  (-> (ni-model (t/ticker->cik "MSFT"))
      :rows-ni)
  (-> (ni-model (t/ticker->cik "MO"))
      ;keys
      add-estimate-existing)
  (-> (ni-model (t/ticker->cik "MO"))
      :combined-ds)

  (->  (z/load-dataset fz (t/ticker->cik "MO"))
       (facts-ds/report-table  :y nil)
       (ds/select-columns [:filed :period  :end "CommonStockDividendsPerShareDeclared"]))

  (-> (ni-model (t/ticker->cik "MO"))
      :combined-ds)


 ; 
  )

