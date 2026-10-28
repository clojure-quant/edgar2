(ns edgar2.algo.growth
  "Top names by stabilized earnings yield compounded three years at stabilized
  earnings growth. Requires at least 7 annual reports, earnings growth
  between 20% and 50%, sales growth of at least 20%, and revenue-score
  and operating-income-score of at least 70."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def n 750)
(def min-score 70)
(def min-annual-years 7)
(def min-earnings-growth 20)
(def max-earnings-growth 50)
(def min-sales-growth 20)

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :annual-count :label "annual-count" :num? true :decimals 0}
   {:key :earnings-yield :label "earnings-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yield :label "earnings-stabilized-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yoy :label "earnings-stabilized-yoy" :num? true :decimals 1}
   {:key :earnings-future-3-yield :label "earnings-future-3-yield" :num? true :decimals 1}
   {:key :rev-score :label "revenue-score" :num? true :decimals 1}
   {:key :oi-score :label "operating-income-score" :num? true :decimals 1}
   {:key :sales-growth-avg-yoy :label "sales-growth-avg-yoy" :num? true :decimals 1}
   {:key :net-income-change-q-on-y :label "net-income-change-q-on-y" :num? true :decimals 1}])

(defn- at-least?
  [row k minimum]
  (and (number? (get row k))
       (>= (double (get row k)) (double minimum))))

(defn- between?
  [row k lo hi]
  (and (number? (get row k))
       (<= (double lo) (double (get row k)) (double hi))))

(defn calculate
  [items _params]
  (let [hits (->> (mapv :row items)
                  (filter #(and (number? (:earnings-future-3-yield %))
                                (at-least? % :annual-count min-annual-years)
                                (between? % :earnings-stabilized-yoy
                                          min-earnings-growth
                                          max-earnings-growth)
                                (at-least? % :sales-growth-avg-yoy min-sales-growth)
                                (at-least? % :revenue-score min-score)
                                (at-least? % :operating-income-score min-score)))
                  (sort-by :earnings-future-3-yield >)
                  (take n)
                  vec)]
    {:rows (mapv report/table-row hits)
     :lines [(format "  annual-count >= %d" min-annual-years)
             (format "  earnings-stabilized-yoy between %d and %d"
                     min-earnings-growth max-earnings-growth)
             (format "  sales-growth-avg-yoy >= %d" min-sales-growth)
             (format "  revenue-score >= %d" min-score)
             (format "  operating-income-score >= %d" min-score)
             (format "  top %d by earnings-future-3-yield" n)
             (format "  %d names" (count hits))]}))

(def screen
  {:id :growth-
   :title "Growth"
   :description ["750 names with the highest stabilized earnings yield compounded three years at stabilized earnings growth, at least 7 annual reports, earnings growth between 20% and 50%, sales-growth-avg-yoy at least 20%, revenue-score at least 70, and operating-income-score at least 70."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
