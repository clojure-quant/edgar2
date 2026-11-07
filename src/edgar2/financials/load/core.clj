(ns edgar2.financials.load.core
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.util.core :as u]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as col]
            [edgar2.financials.load.ixbrl :as xbrl]
            ))

(defn load-facts-facts-api [cik]
  (u/ensure-identity!)
  (u/enable-cache)
  (e/facts cik))

(defn load-facts-filing-api [cik]
  (u/ensure-identity!)
  (u/enable-cache)
  (xbrl/download-filings->ds cik))

(defn load-facts
  ([cik]
   (load-facts cik :facts-api))
  ([cik source]
   (case source
     :facts-api (load-facts-facts-api cik)
     :filings-api (load-facts-filing-api cik)
     )))

(comment 
  ;"SNPMF"
  (load-facts "0001123658" :facts-api)
  (load-facts "0001123658" :filings-api)

  
  
 ; 
  )