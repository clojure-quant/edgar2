(ns demo.net-income
  (:require
   [edgar2.financials.load.zip :as z]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.download.dataset :as dataset]
   [edgar2.db.ticker :refer [primary-ticker-for-cik]])
  (:import [java.time LocalDate]))



(def fz (z/open-facts-zip))

(def ciks (z/cik-list fz))

(defn ni [cik]
  (try 
    (let [facts (z/load-dataset fz cik)
          ds (facts-ds/report-table facts :y nil)
          ni (facts-ds/calc-concept ds :net-income)
          ni (reverse ni)
          ni (take-last 5 ni)
          ticker (-> cik primary-ticker-for-cik :ticker)]
      {:cik cik
       :ticker ticker
       :ni ni})
    (catch Exception _ nil)))

(defn run [{:keys [limit]}]
  (let [input (if limit
                (take limit ciks)
                ciks)
        rows (->> input
                  (map ni)
                  (filter :ticker)
                  (into []))]
    (dataset/write-dataset! "data/ni.edn"
                            {:download-date (str (LocalDate/now))}
                            rows)))



(comment
  (run {:limit 10})
  (run {})
   ;
  )
