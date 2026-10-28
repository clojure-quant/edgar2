(ns edgar2.algo.growth
  "Top names by stabilized earnings yield plus earnings growth,
  with revenue-score and operating-income-score of at least 70."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def n 750)
(def min-score 70)

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :earnings-yield :label "earnings-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yield :label "earnings-stabilized-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yoy :label "earnings-stabilized-yoy" :num? true :decimals 1}
   {:key :earnings-plus-growth-yield :label "earnings-plus-growth-yield" :num? true :decimals 1}
   {:key :rev-score :label "revenue-score" :num? true :decimals 1}
   {:key :oi-score :label "operating-income-score" :num? true :decimals 1}
   {:key :sales-growth-avg-yoy :label "sales-growth-avg-yoy" :num? true :decimals 1}
   {:key :net-income-change-q-on-y :label "net-income-change-q-on-y" :num? true :decimals 1}])

(defn- score-at-least?
  [row k]
  (and (number? (get row k))
       (>= (double (get row k)) min-score)))

(defn calculate
  [items _params]
  (let [hits (->> (mapv :row items)
                  (filter #(and (number? (:earnings-plus-growth-yield %))
                                (score-at-least? % :revenue-score)
                                (score-at-least? % :operating-income-score)))
                  (sort-by :earnings-plus-growth-yield >)
                  (take n)
                  vec)]
    {:rows (mapv report/table-row hits)
     :lines [(format "  revenue-score >= %d" min-score)
             (format "  operating-income-score >= %d" min-score)
             (format "  top %d by earnings-plus-growth-yield" n)
             (format "  %d names" (count hits))]}))

(def screen
  {:id :growth-
   :title "Growth"
   :description ["750 names with the highest stabilized earnings yield plus earnings growth, revenue-score at least 70, and operating-income-score at least 70."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
