(ns edgar2.regression.db
  (:require
   [edgar2.financials.facts-zip :as z]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.regression.net-income-model :as nim]
   [edgar2.db.ticker :as t]
   [tech.v3.dataset :as ds]))

(def fz (z/open-facts-zip))

(defn model-dataset
  "Newest-first dataset of `:filed :period :end :revenue :net-income`.

  Adds `:revenue` and `:net-income` via `calc-concept` to a wide annual
  report-table, drops period-ends where both are non-numeric, and keeps the
  newest 10 rows (report-table is newest-`:end`-first)."
  [wide]
  (let [with-cols (-> (ds/select-columns wide [:filed :period :end])
                      (ds/add-or-update-column
                       :revenue (facts-ds/calc-concept wide :revenue))
                      (ds/add-or-update-column
                       :net-income (facts-ds/calc-concept wide :net-income)))
        kept (->> (ds/rows with-cols {:nil-missing? true})
                  (filter #(or (number? (:revenue %))
                               (number? (:net-income %))))
                  (take 10)
                  vec)]
    (ds/->dataset kept {:dataset-name "ni-model"})))

(defn ni-model [cik]
  (try
    (let [wide (facts-ds/report-table (z/load-dataset fz cik) :y nil)
          ni (->> (facts-ds/calc-concept wide :net-income)
                  reverse
                  (take-last 5))]
      {:cik cik
       :ds (model-dataset wide)
       :ni ni
       :model (nim/classify-series ni)})
    (catch Exception _ nil)))


(comment 
  (require '[edgar2.db.ticker :as t])
  (-> (ni-model (t/ticker->cik "MO")) 
      :model
      
      
      )
  
  
  
 ; 
  )

