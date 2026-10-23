(ns edgar2.algo.price-change
  "Close lookbacks on one bar series.

  Bars are oldest-first. :close is the newest close. :close50 is the
  close 50 rows before that, and the same for 200 and 400. :change50
  is :close / :close50 - 1. Zero means the close did not change. A
  lookback is nil when the series does not reach that far, or the
  earlier close is zero.

  The screen row's :ticker comes from the equity. This function only
  sees the bars."
  (:require [edgar2.screen.universe :as uni]
            [tech.v3.dataset :as ds]))

(def lags
  "Rows back from the newest bar."
  [50 200 400])

(def notes
  ["Close is the newest close."
   "close50, close200, and close400 are that many bars before the newest bar."
   "changeN is close / closeN - 1. Zero means the close did not change."
   "A lookback is nil when the series is shorter than that, or the earlier close is zero."])

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :close :label "Close" :num? true :decimals 2}
   {:key :close50 :label "Close 50" :num? true :decimals 2}
   {:key :close200 :label "Close 200" :num? true :decimals 2}
   {:key :close400 :label "Close 400" :num? true :decimals 2}
   {:key :change50 :label "Chg 50" :num? true :decimals 4}
   {:key :change200 :label "Chg 200" :num? true :decimals 4}
   {:key :change400 :label "Chg 400" :num? true :decimals 4}])

(def label
  "Summary word for one scored series."
  "series")

(defn- finite
  [x]
  (when (and (number? x) (Double/isFinite (double x)))
    (double x)))

(defn- close-at
  "Close at `idx`, or nil when that row is missing or the close is not finite."
  [dataset idx]
  (when (and (nat-int? idx) (< idx (ds/row-count dataset)))
    (finite ((ds/column dataset :close) idx))))

(defn- change
  "close / prior - 1. Nil when `prior` is missing or zero."
  [close prior]
  (when (and close prior (not (zero? prior)))
    (- (/ close prior) 1.0)))

(defn price-change
  "Lookback closes and changes for one oldest-first bar series.

  Returns nil when there is no newest close. The screen adds :ticker."
  [bars]
  (when bars
    (let [dataset (ds/->dataset bars)
          n (ds/row-count dataset)]
      (when (pos? n)
        (when-let [close (close-at dataset (dec n))]
          (let [latest (dec n)]
            (reduce (fn [row lag]
                      (let [prior (close-at dataset (- latest lag))
                            k (keyword (str "close" lag))]
                        (assoc row
                               k prior
                               (keyword (str "change" lag)) (change close prior))))
                    {:close close}
                    lags)))))))

(defn calculate
  "Score each series in `items`. Bars are loaded here.

  Numbers stay numeric (`:raw`) so the report can write them."
  [items _params]
  (if (empty? items)
    {:rows [] :lines []}
    (let [score @(requiring-resolve 'edgar2.eodhd.screen/score)]
      (score items price-change
             {:label label
              :notes notes
              :raw true}))))

(def screen
  {:id :price-change
   :title "Price change"
   :description notes
   :columns columns
   :universe uni/eodhd
   :calculate calculate
   :stamp uni/eodhd-stamp})
