(ns demo.regression-fastmath
  "Demo table for the fastmath OLS models.

  The numeric core lives in `edgar2.regression.fastmath`.

  Run with: clj -X:demo-regression-fastmath"
  (:require [clojure.pprint :as pprint]
            [demo.regression :as demo]
            [edgar2.regression.fastmath :as rf]))

(defn- ols-rows
  [[label vals]]
  [(assoc (rf/trend-linear vals) :ticker label)
   (assoc (rf/trend-linear-pos vals) :ticker label)
   (assoc (rf/trend-log vals) :ticker label)
   (assoc (rf/ols-asinh vals) :ticker label)
   (assoc (rf/stable vals) :ticker label)])

(defn- with-two-decimals
  [row]
  (reduce-kv (fn [m k v]
               (assoc m k (if (number? v)
                            (format "%.2f" (double v))
                            v)))
             {} row))

(defn- format-stat
  "Format `v` with `fmt`, keeping nil and non-finite values readable."
  [fmt v]
  (cond
    (nil? v) "-"
    (not (number? v)) v
    (Double/isNaN (double v)) "NaN"
    (Double/isInfinite (double v)) (if (pos? (double v)) "Inf" "-Inf")
    :else (format fmt (double v))))

(defn- stable-row
  [[label vals]]
  (let [{:keys [trend-slope slope-se t df p-value delta-bic likelihood
                stable?]} (rf/stable vals)]
    {:ticker label
     :trend-slope (format-stat "%.4f" trend-slope)
     :slope-se (format-stat "%.4f" slope-se)
     :t (format-stat "%.4f" t)
     :df (format-stat "%.0f" df)
     :p-value (format-stat "%.4f" p-value)
     :delta-bic (format-stat "%.4f" delta-bic)
     :likelihood (format-stat "%.4f" likelihood)
     :stable? stable?}))

(defn run
  "Print the OLS fit of every series in `demo.regression/cases`."
  ([] (run {}))
  ([_]
   (println "OLS with fastmath")
    (pprint/print-table [:ticker :model :intercept :slope :r-squared
                         :adj-r-squared :stabilized-yoy :stabilized]
                        (mapv with-two-decimals
                              (mapcat ols-rows demo/cases)))
    (println "\nStable-input likelihood")
    (pprint/print-table [:ticker :trend-slope :slope-se :t :df :p-value
                         :delta-bic :likelihood :stable?]
                        (mapv stable-row demo/cases))))

(defn -main
  [& _]
  (run))

(comment
  (run)
  ;
  )
