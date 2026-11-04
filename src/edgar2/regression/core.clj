(ns edgar2.regression.core
  "Dispatcher for the earnings regression fits."
  (:require [edgar2.regression.log :as log]
            [edgar2.regression.linear :as linear]
            [edgar2.regression.asinh :as asinh]))

(defn- unknown-type
  [regression-type]
  (throw (IllegalArgumentException.
          (str "Unknown :regression-type " regression-type
               ", expected :log, :linear, or :asinh"))))

(defn- fit-fn
  [regression-type]
  (case regression-type
    :log log/regression
    :linear linear/regression
    :asinh asinh/regression
    (unknown-type regression-type)))

(defn- estimate-fn
  [regression-type]
  (case regression-type
    :log log/estimate
    :linear linear/estimate
    :asinh asinh/estimate
    (unknown-type regression-type)))

(defn- slope-percent-fn
  [regression-type]
  (case regression-type
    :log log/slope-percent
    :linear linear/slope-percent
    :asinh asinh/slope-percent
    (unknown-type regression-type)))

(defn regression
  "Fit earnings with the regression named by `:regression-type`.

  Takes one map: `{:regression-type :log|:linear|:asinh, :error
  :square|:absolute, :opts ..., :v ...}`. `:error` defaults to `:square`.
  Returns the raw regression map, or nil when the fit has too few points.
  Throws IllegalArgumentException for an unknown or missing
  `:regression-type`.

  Also takes `[opts vals]`, where `opts` holds the same keys except `:v`
  and `vals` is the oldest-first earnings vector."
  ([{:keys [regression-type error opts v]}]
   (let [fit (fit-fn regression-type)]
     (fit (merge opts {:error (or error (:error opts) :square)}) v)))
  ([opts vals]
   (regression (assoc opts :v vals))))

(defn estimate
  "Fitted earnings at `offset` periods past the last input index.

  Dispatches on `:regression-type` in `opts`, which takes the same map as
  `regression`. Offset 0 is the current fitted value, 1 is one period ahead.
  Nil when `regression` is nil."
  [opts regression offset]
  ((estimate-fn (:regression-type opts)) regression offset))

(defn slope-percent
  "Fitted growth as a percent.

  Dispatches on `:regression-type` in `opts`, which takes the same map as
  `regression`. Nil when `regression` is nil."
  [opts regression]
  ((slope-percent-fn (:regression-type opts)) regression))
