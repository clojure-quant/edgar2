(ns demo.report-table
  (:require 
   [clojure.string :as str]
   [clojure.pprint]
   [tech.v3.dataset :as ds]
   [tablecloth.api :as tc]
   [edgar.api :as e]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.util :refer [resolve-filer2]]))

(def demo-fields
  [[:dei :EntityCommonStockSharesOutstanding]
   [:us-gaap :CostOfGoodsAndServicesSold]
   [:us-gaap :CommonStockSharesOutstanding]
   [:ifrs-full :DividendsPaidOrdinarySharesPerShare]
   ;[:us-gaap :PaymentsForRepurchaseOfCommonStock]
   [:us-gaap :Revenues]
   ])

(comment
  (e/init! "Your Name your@email.com")
  
  (def dss (e/facts (resolve-filer2 "XOM") ))
  
  (def dss (e/facts "MSFT"))

  (def dss (e/facts "SONY"))

  ; taxonomy cef
  (def dss (e/facts "ACP"))
  
  (ds/filter dss #(= "cef" (str (:taxonomy %))))
  
  (def dss (e/facts "TAK"))

  dss
  (defn f [row]
    (let [s (:concept row)]
      (and (not (nil? s))
           (not (str/blank? s)) 
           (str/includes? (str/lower-case s) "dividend")
           )))

  (-> dss
      (ds/filter f)
      (ds/filter #(= "2025-06-25" (:filed %)))
      (tc/select-columns [:filed :concept :val])
      (ds/print-all))
  

  (facts-ds/annual-periods dss)
  (facts-ds/quarterly-periods dss)

  (facts-ds/report-table dss :y demo-fields)
  (facts-ds/report-table dss :q demo-fields)

  (facts-ds/annual-periods dss)
  (facts-ds/quarterly-periods dss)

  (-> dss
      (ds/select-columns facts-ds/period-cols)
      (tc/unique-by facts-ds/period-cols)
      (tc/order-by :filed)
      (tc/select-rows (fn [row] (= (:form row) "10-K")))
      (tc/select-rows (fn [row] (= (:fy row) 2025)))
      (tc/order-by :end))

  (-> dss
      (tc/select-rows (fn [row] (= (:form row) "10-K")))
      (tc/select-rows (fn [row] (or (= (:fy row) 2025)
                                    (= (:fy row) 2024)
                                    (= (:fy row) 2023))))
      (ds/select-columns [:filed :frame :end :taxonomy :concept :val])
      (tc/select-rows (fn [row] (= (:concept row) "OperatingIncomeLoss")))
      (tc/order-by :filed))
  )
