(ns edgar2.algo.high-revenue-growth
  "The 50 highest 7-year mean YoY sales-growth names in the universe."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def n 50)

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :price :label "Price" :num? true :decimals 2}
   {:key :mc :label "MC" :num? true}
   {:key :s-yoy :label "Sales YoY" :num? true :decimals 1}
   {:key :dyld :label "Yield" :num? true}])

(defn calculate
  [items _params]
  (let [hits (->> (mapv :row items)
                  (filter #(number? (:sales-growth-yoy %)))
                  (sort-by :sales-growth-yoy >)
                  (take n)
                  vec)]
    {:rows (mapv report/table-row hits)
     :lines [(format "  %d names with the highest 7-year mean YoY sales-growth" n)
             (format "  %d names" (count hits))]}))

(def screen
  {:id :high-revenue-growth
   :title "High revenue growth"
   :description ["50 names with the highest 7-year mean YoY sales growth."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
