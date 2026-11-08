(ns edgar2.financials.load.core
  (:require [edgar.api :as e]
            [edgar2.util.core :as u]
            [edgar2.financials.load.ixbrl :as xbrl]
            [edgar2.financials.load.zip :as facts-zip]))

(defn load-facts-facts-api [cik]
  (u/ensure-identity!)
  (u/enable-cache)
  (e/facts cik))

(defn load-facts-filing-api [cik]
  (u/ensure-identity!)
  (u/enable-cache)
  (xbrl/download-filings->ds cik))

(defn load-facts-zip [cik]
  (with-open [zf (facts-zip/open-facts-zip)]
    (facts-zip/load-dataset zf cik)))

(defn load-facts
  ([cik]
   (load-facts cik :facts-api))
  ([cik source]
   (case source
     :facts-api (load-facts-facts-api cik)
     :filings-api (load-facts-filing-api cik)
     :facts-zip (load-facts-zip cik)
     )))

(comment 
  ;"SNPMF"
  (load-facts "0001123658" :facts-api)
  (load-facts "0001123658" :filings-api)
  (load-facts "0001123658" :facts-zip)
  
  
 ; 
  )