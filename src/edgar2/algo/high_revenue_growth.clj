(ns edgar2.algo.high-revenue-growth
  "The highest 7-year mean YoY sales-growth names in the universe.
  The name count and Min mc are screen parameters."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def top-n-choices
  [10 25 50 100 200])

(def top-n
  (util/choice-param :top-n "Top n" top-n-choices 50))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :price :label "Price" :num? true :decimals 2}
   {:key :mc :label "MC" :num? true}
   {:key :s-yoy :label "Sales YoY" :num? true :decimals 1}
   {:key :dyld :label "Yield" :num? true}])

(defn calculate
  [items params]
  (let [n (util/param-value params top-n)
        result (util/apply-and-pipeline
                (uni/calculate-stats items)
                [(uni/min-mc-step (util/param-value params uni/min-mc))
                 {:label "sales-growth-avg-yoy numeric"
                  :pred (util/number-at :sales-growth-avg-yoy)}])
        hits (->> (:tickers result)
                  (sort-by :sales-growth-avg-yoy >)
                  (take n)
                  vec)]
    {:rows (mapv report/table-row hits)
     :lines (conj (util/report-pipeline-info result)
                  (format "top %d by 7-year mean YoY sales-growth (showing %d)"
                          n (count hits)))}))

(def screen
  {:id :high-revenue-growth
   :title "High revenue growth"
   :description ["Top names by 7-year mean YoY sales growth above Min mc. The name count and Min mc are parameters."]
   :params [top-n uni/min-mc]
   :columns columns
   :universe uni/stats-universe-all
   :calculate calculate
   :stamp uni/stats-stamp})
