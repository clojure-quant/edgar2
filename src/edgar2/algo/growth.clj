(ns edgar2.algo.growth
  "Top names by stabilized earnings yield plus earnings growth."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def n 750)

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :earnings-yield :label "earnings-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yield :label "earnings-stabilized-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yoy :label "earnings-stabilized-yoy" :num? true :decimals 1}
   {:key :earnings-plus-growth-yield :label "earnings-plus-growth-yield" :num? true :decimals 1}
   {:key :sales-growth-avg-yoy :label "sales-growth-avg-yoy" :num? true :decimals 1}
   {:key :net-income-change-q-on-y :label "net-income-change-q-on-y" :num? true :decimals 1}])

(defn calculate
  [items _params]
  (let [hits (->> (mapv :row items)
                  (filter #(number? (:earnings-plus-growth-yield %)))
                  (sort-by :earnings-plus-growth-yield >)
                  (take n)
                  vec)]
    {:rows (mapv report/table-row hits)
     :lines [(format "  top %d by earnings-plus-growth-yield" n)
             (format "  %d names" (count hits))]}))

(def screen
  {:id :growth-
   :title "Growth"
   :description ["750 names with the highest stabilized earnings yield plus earnings growth."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
