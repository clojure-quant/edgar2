(ns edgar2.financials.load.core
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.util.core :as u]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as col]
            [edgar2.financials.load.ixbrl :as xbrl]
            ))

(defn load-facts-api [cik]
  (u/ensure-identity!)
  (u/enable-cache)
  (e/facts cik))

(defn load-filings-api [cik]
  (u/ensure-identity!)
  (u/enable-cache)
  (xbrl/download-filings->ds cik))

(defn load-filings 
  ([cik]
   (load-filings cik :facts-api))
  ([cik source]
   (case source
     :facts-api (load-facts-api cik)
     :filings-api (load-filings-api cik)
     )))

(comment 
  ;"SNPMF"
  (load-facts-api "0001123658")
  (load-filings-api "0001123658")

  
  
 ; 
  )