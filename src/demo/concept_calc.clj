(ns demo.concept-calc
  "Demo: calc-concepts on an annual report-table for one ticker."
  (:require
   [tech.v3.dataset :as ds]
   [tablecloth.api :as tc]
   [edgar.api :as e]
   [edgar2.util :as util]
   [edgar2.util.core :as ucore]
   [edgar2.db.ticker :as t]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.financials.load.core :refer [load-facts]]
   [edgar2.financials.calc.concept :as calc]
   [edgar2.financials.calc.current.core :as cur]
   [edgar2.financials.calc.current.growth :as growth]))

(def show-columns
  [:filed :period :end
   ; :revenue :net-income
   ;:ebit
   ;:ebitda 
   :net-income-margin
   ;:depreciation-amortization
   ;:depreciation
   ;:amortization
   :roa
   :roce
   :dividend-buyback-coverage
   :dividends-paid])

(defn run
  "Load demo ticker facts, print calc-concepts table."
  ([{:keys [ticker cik]}]
   (let [cik (if cik
               cik
               (t/ticker->cik ticker))
         ticker (if ticker
                  ticker
                  (t/primary-ticker-for-cik cik))
         facts (load-facts cik)
         a (facts-ds/report-table facts :y nil)
         cc (calc/calc-concepts a)

         ;cc (growth/calc-growth cc)
         ]
     (println "\n ticker: " ticker " cik: " cik "reports: " (tc/row-count a))
     ;(println cc)
     (println (ds/select-columns cc show-columns))
     (println (cur/calculate-annual facts)))))

(comment
  (run {:ticker "MSFT"})

  (run {:cik "0000055234"}) ; not 7 years of data

  (run {:cik "0000066496"})
  (run {:ticker "SNPMF"})






  ;
  )
