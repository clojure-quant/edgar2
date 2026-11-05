(ns edgar2.regression.net-income-model
  "Net-income series classification: stable vs. linear trend."
  (:require [demo.regression-fastmath :as rf]))

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
        (if (:stable? s) s (rf/trend-linear vals))))))

(defn trend-significant?
  "True when a trend row's fit reaches `r-squared-significant`."
  [row]
  (let [r-squared (:r-squared row)]
    (and (number? r-squared) (<= r-squared-significant (double r-squared)))))
