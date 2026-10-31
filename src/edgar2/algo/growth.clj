(ns edgar2.algo.growth
  "Names sorted by stabilized earnings yield compounded three years at
  stabilized earnings growth. Default cuts: at least 7 annual reports,
  earnings growth between 12% and 65%, sales growth of at least 12%,
  revenue-score of at least 65, operating-income-score of at least 70,
  then market cap at or above Min mc. Each cut is a screen parameter."
  (:require [clojure.string :as str]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def year-choices
  [1 2 3 4 5 6 7 8 9 10 12 15 20])

(def growth-choices
  [0 5 8 10 12 15 20 25 30 40 50 65 80 100 150 200])

(def score-choices
  [0 20 40 50 60 65 70 75 80 85 90 95 100])

(defn- as-long
  "Long value of `x`, or nil. Accepts a number or a decimal string."
  [x]
  (cond
    (integer? x) (long x)
    (number? x) (long x)
    (string? x) (parse-long (str/trim x))
    :else nil))

(defn- parse-choice
  "One of `choices`, or nil. Accepts the number or its string."
  [choices x]
  (let [n (as-long x)]
    (when (some #{n} choices) n)))

(defn- choice-param
  [key label choices default]
  {:key key
   :label label
   :choices choices
   :default default
   :parse (fn [x] (parse-choice choices x))})

(def min-annual-years
  (choice-param :min-annual-years "Min annual years" year-choices 7))

(def min-earnings-growth
  (choice-param :min-earnings-growth "Min earnings growth" growth-choices 12))

(def max-earnings-growth
  (choice-param :max-earnings-growth "Max earnings growth" growth-choices 65))

(def min-sales-growth
  (choice-param :min-sales-growth "Min sales growth" growth-choices 12))

(def min-revenue-score
  (choice-param :min-revenue-score "Min revenue score" score-choices 65))

(def min-operating-income-score
  (choice-param :min-operating-income-score "Min operating income score"
                score-choices 70))

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
   {:key :net-income-change-q-on-y :label "net-income-change-q-on-y" :num? true :decimals 1}
   {:key :earnings-variation :label "earnings-variation" :num? true :decimals 1}])

(defn- param-value
  "Value of `param` in `params`, or that param's default."
  [params {:keys [key default]}]
  (let [v (get params key)]
    (if (some? v) v default)))

(defn filter-ands
  "Growth cuts, then market cap. Thresholds come from `params`."
  [params]
  (let [min-years (param-value params min-annual-years)
        min-earn (param-value params min-earnings-growth)
        max-earn (param-value params max-earnings-growth)
        min-sales (param-value params min-sales-growth)
        min-rev (param-value params min-revenue-score)
        min-oi (param-value params min-operating-income-score)
        min-mc (param-value params uni/min-mc)]
    [{:label (format "annual-count >= %d" min-years)
      :pred (util/at-least :annual-count min-years)}
     {:label (format "earnings-stabilized-yoy between %d and %d"
                     min-earn max-earn)
      :pred (util/between :earnings-stabilized-yoy min-earn max-earn)}
     {:label (format "sales-growth-avg-yoy >= %d" min-sales)
      :pred (util/at-least :sales-growth-avg-yoy min-sales)}
     {:label (format "revenue-score >= %d" min-rev)
      :pred (util/at-least :revenue-score min-rev)}
     {:label (format "operating-income-score >= %d" min-oi)
      :pred (util/at-least :operating-income-score min-oi)}
     {:label (format "mc >= %d ($ millions)" min-mc)
      :pred (fn [row]
              (seq (report/at-least-mc [row] min-mc)))}]))

(defn- future-yield
  [row]
  (let [y (:earnings-future-3-yield row)]
    (when (number? y) (double y))))

(defn calculate
  [items params]
  (let [result (util/apply-and-pipeline (mapv :row items)
                                        (filter-ands params))
        hits (vec (sort-by #(or (future-yield %) Double/NEGATIVE_INFINITY)
                           >
                           (:tickers result)))]
    {:rows (mapv report/table-row hits)
     :lines (conj (util/report-pipeline-info result) sort-line)}))

(def screen
  {:id :growth-
   :title "Growth"
   :description ["At least 7 annual reports, stabilized earnings growth between 12% and 65%, sales-growth-avg-yoy at least 12%, revenue-score at least 65, operating-income-score at least 70, then market cap at or above Min mc, sorted by earnings-future-3-yield. Each threshold is a parameter."]
   :params [min-annual-years
            min-earnings-growth
            max-earnings-growth
            min-sales-growth
            min-revenue-score
            min-operating-income-score
            uni/min-mc]
   :columns columns
   :universe uni/stats-all
   :calculate calculate
   :stamp uni/stats-stamp})
