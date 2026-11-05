(ns edgar2.regression.db
  (:require
   [edgar2.financials.facts-zip :as z]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.regression.net-income-model :as nim]
   [edgar2.db.ticker :as t]))

(def fz (z/open-facts-zip))

(defn ni-model [cik]
  (try
    (let [facts (z/load-dataset fz cik)
          ds (facts-ds/report-table facts :y nil)
          ni (facts-ds/calc-concept ds :net-income)
          ni (reverse ni)
          ni (take-last 5 ni)]
      {:cik cik
       :ds ds
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

