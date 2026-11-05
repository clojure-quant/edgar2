(ns edgar2.regression.net-income-model
  "Net-income series classification: stable vs. linear trend."
  (:require [demo.regression-fastmath :as rf]
            [tech.v3.dataset :as ds]))

(def ^:private r-squared-significant
  "A trend is significant when its `:r-squared` reaches this value."
  0.8)

(defn classify-series
  "Stable model for `vals` when it has no trend, otherwise the linear
  trend model. Nil when `vals` has no observations.

  `data/ni.edn` stores missing years as nil; those are dropped before
  fitting, since the series carries no year labels to preserve."
  [vals]
  (let [vals (remove nil? vals)]
    (when (seq vals)
      (let [s (rf/stable vals)]
        (-> (if (:stable? s) s (rf/trend-linear vals))
            (assoc :n (count vals)))))))

(defn trend-significant?
  "True when a trend row's fit reaches `r-squared-significant`."
  [row]
  (let [r-squared (:r-squared row)]
    (and (number? r-squared) (<= r-squared-significant (double r-squared)))))




(defn predict [{:keys [model slope stabilized intercept]} idx]
  ; :model	:trend-linear
  ; http://localhost:3000/company/net-income-model?ticker=MO
  ; slope	1.4444E9
  ; stabilized	9.8048E9

  ; model	stable
  ; stabilized	1.744E8

  (case model
    :stable
    stabilized
    :trend-linear
    (+ intercept (* slope idx))
    ; else
    0.0))

(defn predict-past [{:keys [n] :as row}]
  (map (partial predict row) (range n)))

(defn predict-future [{:keys [n] :as row} n-future]
  (map (partial predict row)
       (range (+ n n-future -1) (- n 1) -1)))





