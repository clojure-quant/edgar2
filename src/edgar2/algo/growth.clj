(ns edgar2.algo.growth
  "Top names by earnings-average yield plus earnings growth."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def n 750)

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :earnings-yield :label "earnings-yield" :num? true :decimals 1}
   {:key :earnings-avg-yield :label "earnings-avg-yield" :num? true :decimals 1}
   {:key :earnings-growth-avg-yoy :label "earnings-growth-avg-yoy" :num? true :decimals 1}
   {:key :earnings-plus-growth-avg-yield :label "earnings-plus-growth-avg-yield" :num? true :decimals 1}
   {:key :sales-growth-avg-yoy :label "sales-growth-avg-yoy" :num? true :decimals 1}
   {:key :net-income-change-q-on-y :label "net-income-change-q-on-y" :num? true :decimals 1}])

(defn calculate
  [items _params]
  (let [hits (->> (mapv :row items)
                  (filter #(number? (:earnings-plus-growth-avg-yield %)))
                  (sort-by :earnings-plus-growth-avg-yield >)
                  (take n)
                  vec)]
    {:rows (mapv report/table-row hits)
     :lines [(format "  top %d by earnings-plus-growth-avg-yield" n)
             (format "  %d names" (count hits))]}))

(def screen
  {:id :growth-
   :title "Growth"
   :description ["750 names with the highest earnings-average yield plus earnings growth."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
