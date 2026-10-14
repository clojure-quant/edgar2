(ns edgar2.algo.cheap-consistent-margin
  "Bottom 50% PE, top 30% revenue-score and operating-income-score, top 30% margin."
  (:require [edgar2.algo.cheap-consistent :as consistent]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :price :label "Price" :num? true :decimals 2}
   {:key :mc :label "MC" :num? true}
   {:key :pe :label "PE" :num? true :decimals 1}
   {:key :rev-score :label "Rev score" :num? true :decimals 1}
   {:key :oi-score :label "OI score" :num? true :decimals 1}
   {:key :avg-margin :label "Avg margin" :num? true}
   {:key :dyld :label "Yield" :num? true}])

(defn calculate
  [items _params]
  (let [result (consistent/cheap-consistent-on (mapv :row items) 0.50 0.30)]
    {:rows (mapv report/table-row (:rows result))
     :lines (consistent/consistent-lines result)}))

(def screen
  {:id :cheap-consistent-margin
   :title "Cheap consistent margin"
   :description ["Bottom 50% PE (positive only), top 30% revenue-score, top 30% operating-income-score, top 30% avg-profit-margin."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
