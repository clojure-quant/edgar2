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
   [edgar2.financials.facts-zip :as facts-zip]
   [edgar2.financials.calc.concept :as calc]
   [edgar2.financials.calc.current.core :as cur]
   [edgar2.financials.calc.current.growth :as growth]))

(defn load-facts-edgar
  "Companyfacts for `ticker` (XOM via resolve-filer2)."
  [ticker]
  (ucore/ensure-identity!)
  (e/facts (util/resolve-filer2 ticker)))

(defn load-facts-zip [cik]
  (with-open [zf (facts-zip/open-facts-zip)]
    (facts-zip/load-dataset zf cik)))


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
   :dividend-buyback-coverage])

(defn run
  "Load demo ticker facts, print calc-concepts table."
  ([{:keys [ticker cik]}]
   (let [cik (if cik
               cik
               (t/ticker->cik ticker))
         ticker (if ticker
                  ticker
                  (t/primary-ticker-for-cik cik))
         facts (load-facts-zip cik)
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

  



  ;
  )
