(ns edgar2.algo.growth
  "Names sorted by stabilized earnings yield compounded three years at
  stabilized earnings growth. Keeps at least 7 annual reports, earnings
  growth between 12% and 65%, sales growth of at least 12%, revenue-score
  of at least 65, and operating-income-score of at least 70."
  (:require [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def min-annual-years 7)
(def min-earnings-growth 12)
(def max-earnings-growth 65)
(def min-sales-growth 12)
(def min-revenue-score 65)
(def min-operating-income-score 70)

(def sort-line
  "take this list and sort by earnings-future-3-yield")

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :annual-count :label "annual-count" :num? true :decimals 0}
   {:key :earnings-yield :label "earnings-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yield :label "earnings-stabilized-yield" :num? true :decimals 1}
   {:key :earnings-stabilized-yoy :label "earnings-stabilized-yoy" :num? true :decimals 1}
   {:key :earnings-future-3-yield :label "earnings-future-3-yield" :num? true :decimals 1}
   {:key :rev-score :label "revenue-score" :num? true :decimals 1}
   {:key :oi-score :label "operating-income-score" :num? true :decimals 1}
   {:key :sales-growth-avg-yoy :label "sales-growth-avg-yoy" :num? true :decimals 1}
   {:key :net-income-change-q-on-y :label "net-income-change-q-on-y" :num? true :decimals 1}])

(def filter-ands
  [{:label (format "annual-count >= %d" min-annual-years)
    :pred (util/at-least :annual-count min-annual-years)}
   {:label (format "earnings-stabilized-yoy between %d and %d"
                   min-earnings-growth max-earnings-growth)
    :pred (util/between :earnings-stabilized-yoy
                        min-earnings-growth max-earnings-growth)}
   {:label (format "sales-growth-avg-yoy >= %d" min-sales-growth)
    :pred (util/at-least :sales-growth-avg-yoy min-sales-growth)}
   {:label (format "revenue-score >= %d" min-revenue-score)
    :pred (util/at-least :revenue-score min-revenue-score)}
   {:label (format "operating-income-score >= %d" min-operating-income-score)
    :pred (util/at-least :operating-income-score min-operating-income-score)}])

(defn- future-yield
  [row]
  (let [y (:earnings-future-3-yield row)]
    (when (number? y) (double y))))

(defn calculate
  [items _params]
  (let [result (util/apply-and-pipeline (mapv :row items) filter-ands)
        hits (vec (sort-by #(or (future-yield %) Double/NEGATIVE_INFINITY)
                           >
                           (:tickers result)))]
    {:rows (mapv report/table-row hits)
     :lines (conj (util/report-pipeline-info result) sort-line)}))

(def screen
  {:id :growth-
   :title "Growth"
   :description ["At least 7 annual reports, stabilized earnings growth between 12% and 65%, sales-growth-avg-yoy at least 12%, revenue-score at least 65, and operating-income-score at least 70, sorted by earnings-future-3-yield."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
