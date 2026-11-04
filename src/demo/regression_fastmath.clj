(ns demo.regression-fastmath
  "Ordinary least squares estimation with fastmath.

  Run with: clj -X:demo-regression-fastmath"
  (:require [clojure.pprint :as pprint]
            [demo.regression :as demo]
            [fastmath.stats :as stats]))

(defn ols
  "Least-squares line through `vals` against its position.

  Returns `{:intercept :slope :r-squared}`, where `:r-squared` is the
  centered coefficient of determination."
  [vals]
  (let [xs (mapv double (range (count vals)))
        ys (mapv double vals)
        x-bar (stats/mean xs)
        y-bar (stats/mean ys)
        slope (/ (stats/covariance xs ys) (stats/variance xs))
        intercept (- y-bar (* slope x-bar))
        fitted (mapv (fn [x] (+ intercept (* slope x))) xs)
        ss-res (reduce + (map (fn [y f]
                                (let [d (- y f)] (* d d)))
                              ys fitted))
        ss-tot (reduce + (map (fn [y]
                                (let [d (- y y-bar)] (* d d)))
                              ys))]
    {:intercept intercept
     :slope slope
     :r-squared (if (zero? (double ss-tot))
                  (if (zero? (double ss-res)) 1.0 0.0)
                  (- 1.0 (/ ss-res ss-tot)))
     :xs xs
     :y-bar y-bar}))

(defn stabilized [{:keys [slope intercept xs]}]
  (+ intercept (* slope (last xs))))

(defn ols-linear [vals]
  (let [{:keys [slope y-bar] :as s} (ols vals)]
    (assoc s
           :model :ols-linear
           :stabilized-yoy (/ slope y-bar)
           :stabilized (stabilized s))))

(defn ols-linear-pos [vals]
  (assoc (ols-linear (filter pos? vals)) :model :ols-linear-pos))

(defn ols-log [vals]
  (let [vals (->> vals
                  (filter pos?)
                  (map Math/log))
        {:keys [slope y-bar] :as s} (ols vals)]
    (assoc s
           :model :ols-log
           :stabilized-yoy (/ slope y-bar)
           :stabilized (-> s stabilized Math/exp))))

(defn- asinh
  [x]
  (let [x (double x)
        ax (Math/abs x)
        m (Math/log (+ ax (Math/sqrt (inc (* ax ax)))))]
    (if (neg? x) (- m) m)))

(defn ols-asinh [vals]
  (let [vals (->> vals
                  (map asinh))
        {:keys [slope y-bar] :as s} (ols vals)]
    (assoc s
           :model :ols-asinh
           ;:stabilized-yoy (/ slope y-bar)
           ;:stabilized (-> s stabilized Math/exp)
           )))


(defn- ols-rows
  [[label vals]]
  [(assoc (ols-linear vals) :ticker label)
   (assoc (ols-linear-pos vals) :ticker label)
   (assoc (ols-log vals) :ticker label)
   (assoc (ols-asinh vals) :ticker label)])

(defn- with-two-decimals
  [row]
  (reduce-kv (fn [m k v]
               (assoc m k (if (number? v)
                            (format "%.2f" (double v))
                            v)))
             {} row))

(defn run
  "Print the OLS fit of every series in `demo.regression/cases`."
  ([] (run {}))
  ([_]
   (println "OLS with fastmath")
   (pprint/print-table [:ticker :model :intercept :slope :r-squared
                        :stabilized-yoy :stabilized]
                       (mapv with-two-decimals
                             (mapcat ols-rows demo/cases)))))

(defn -main
  [& _]
  (run))

(comment
  (run)
  ;
  )