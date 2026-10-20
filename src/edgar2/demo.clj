(ns edgar2.demo
  (:require [edgar.api :as e]
            [edgar2.facts-ds :as facts-ds]
            [tablecloth.api :as tc]
            [tech.v3.dataset :as ds]
            [edgar2.filing :refer [resolve-filer]]
            ))

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
  
  (def dss (e/facts (resolve-filer "XOM") ))
  
  (def dss2 (e/facts "MSFT"))

  (def dss2 (e/facts "SONY"))
  
  dss
  
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
