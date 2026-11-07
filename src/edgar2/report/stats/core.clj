(ns edgar2.report.stats.core
  (:require
   [edgar2.db.ticker :as t]
   [edgar2.db.universe :as db-universe]
   [edgar2.db.price-change :as db-price-change]
   [edgar2.db.shares :as db-shares]
   [edgar2.db.sic :as db-sic]
   [edgar2.financials.facts-zip :as facts-zip]
   [edgar2.financials.calc.current.core :as cur]
   [edgar2.report.stats.valuation :as valuation] 
   [edgar2.regression.db :as regression]
   ))


(defn load-facts-zip [cik]
  (with-open [zf (facts-zip/open-facts-zip)]
    (facts-zip/load-dataset zf cik)))

(defn universe-data-cols
  "Identity / reporting-currency fields from a universe-facts `row`."
  [row]
  (select-keys row [:ticker :exchange :cik :entityName :report-currency]))

(defn universe [cik]
  (-> (db-universe/universe-row-by-cik cik) 
      (universe-data-cols)))


(defn stats-cik [cik]
  (let [ticker (t/primary-ticker-for-cik cik)
        ; facts
        facts (load-facts-zip cik)
        annual-props (cur/calculate-annual facts)
        regression-props (regression/ni-model-facts facts)
        facts-props (merge annual-props regression-props)
        desc (merge  {:cik cik :ticker ticker}
                     (universe cik)
                     (db-sic/sic-data-for-cik cik))
        facts-desc (merge facts-props desc)
        shares (db-shares/get-shares-cik cik)]
    (merge    
     facts-desc
     (db-price-change/get-change ticker)
     (valuation/calculate shares facts-desc))))

(defn stats-ticker [ticker]
  (let [cik (t/ticker->cik ticker)]
    (stats-cik cik)))


(defn clean [row]
  (dissoc row 
          :all-ds
          :combined-ds
          :model-ds
          :ni 
          :rows-ni
          ))


(comment 
  
  (-> "MO"
      (stats-ticker)
      (clean)
      println
      )
  
  (-> "0000066496"
      (stats-cik))
  
 ; 
  )

