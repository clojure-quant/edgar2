(ns edgar2.algo.cheap-consistent-margin
  "Bottom fraction of PE, top fractions of revenue-score,
  operating-income-score and avg-profit-margin. Each fraction and
  Min mc is a screen parameter."
  (:require [edgar2.algo.cheap-consistent :as consistent]
            [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

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

(def pe-fraction
  (util/choice-param :pe-fraction "PE fraction"
                     cutoff/fraction-choices 0.50))

(defn calculate
  [items params]
  (let [result (consistent/cheap-consistent-on
                (uni/calculate-stats items)
                {:min-mc (util/param-value params uni/min-mc)
                 :pe-p (util/param-value params pe-fraction)
                 :rev-p (util/param-value params consistent/revenue-score-fraction)
                 :oi-p (util/param-value params consistent/operating-income-score-fraction)
                 :margin-p (util/param-value params consistent/margin-fraction)})]
    {:rows (mapv report/table-row (:rows result))
     :lines (:lines result)}))

(def screen
  {:id :cheap-consistent-margin
   :title "Cheap consistent margin"
   :description ["Bottom fraction of PE (positive only), top fractions of revenue-score, operating-income-score and avg-profit-margin. Each fraction and Min mc is a parameter."]
   :params [pe-fraction
            consistent/revenue-score-fraction
            consistent/operating-income-score-fraction
            consistent/margin-fraction
            uni/min-mc]
   :columns columns
   :universe uni/stats-universe-all
   :calculate calculate
   :stamp uni/stats-stamp})
