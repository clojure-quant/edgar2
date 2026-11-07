(ns edgar2.financials.calc.current.core
  "Annual fundamentals from a companyfacts dataset."
  (:require
   [tech.v3.dataset :as ds]
   [tablecloth.api :as tc]
   [edgar2.financials.facts-ds :as facts-ds]
   [edgar2.financials.calc.concept :as calc-concept]
   [edgar2.financials.calc.current.growth :as growth]
   [edgar2.financials.calc.current.quarterly :as q]
  ))

(defn most-recent-row [ds]
  (-> ds
      (tc/select-rows 0)
      (tc/rows :as-maps)
      first))


(defn calculate-annual
  "Fundamentals from an `e/facts`-shaped dataset via `report-table`."
  [dss]
  (let [a-raw (facts-ds/report-table dss :y nil)
        q-raw (facts-ds/report-table dss :q nil)
        a (calc-concept/calc-concepts a-raw)
        q (calc-concept/calc-concepts q-raw)
        a-most-recent (most-recent-row a)
        rows (tc/row-count a)
        g (growth/calc-growth a)]
    (-> a-most-recent
        (assoc :annual-count rows)
        (merge g)
        (merge (q/calculate-quarterly q)))))


(comment
  (-> (tc/dataset {:a 1 :b 2} {:a 2 :b -3})
      (tc/select-rows 0)
      (tc/rows :as-maps)
      first)
  ;
  )

