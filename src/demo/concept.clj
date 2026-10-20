(ns demo.concept
  "Demo: calc-concept :revenue on annual report-tables for XOM, BP, SONY."
  (:require [edgar.api :as e]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.filing :as filing]
            [tech.v3.dataset :as ds]))

(def tickers ["XOM" "BP" "SONY"])

(defn load-facts
  "Companyfacts for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (e/facts (filing/resolve-filer2 ticker)))

(defn with-revenue
  "Annual report-table for all :revenue tags, plus :revenue-c from calc-concept."
  [facts y-or-q]
  (let [fields (:data (concepts/get-concept-id :revenue))
        table (facts-ds/report-table facts y-or-q fields)
        revenue-c (facts-ds/calc-concept table :revenue)]
    (ds/add-or-update-column table :revenue-c revenue-c)))

(defn run
  "Load XOM / BP / SONY facts, add :revenue-c, print each table."
  []
  (filing/ensure-identity!)
  (doseq [ticker tickers]
    (let [f (load-facts ticker)
          table (with-revenue f :y)
          table-q (with-revenue f :q)]
      (println "\n=== y " ticker "===")
      (println table)
      (println "\n=== q " ticker "===")
      (println table))))

(defn -main
  [& _]
  (run)
  ;
  )
