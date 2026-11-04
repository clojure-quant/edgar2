(ns demo.regression-fastmath
  "Ordinary least squares estimation with fastmath.

  Run with: clj -X:demo-regression-fastmath"
  (:require [clojure.pprint :as pprint]
            [demo.regression :as demo]
            [fastmath.random :as r]
            [fastmath.stats :as stats]))

(defn- adjusted-r-squared [r-squared n k]
  (if (<= n (inc k))
    0.0
    (- 1.0 (* (- 1.0 (double r-squared))
              (/ (dec n) (- n k 1))))))

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
                              ys))
        r-squared (if (zero? (double ss-tot))
                    (if (zero? (double ss-res)) 1.0 0.0)
                    (- 1.0 (/ ss-res ss-tot)))]
    {:intercept intercept
     :slope slope
     :r-squared r-squared
     :adj-r-squared (adjusted-r-squared r-squared (count xs) 1)
     :xs xs
     :y-bar y-bar}))

(defn stabilized [{:keys [slope intercept xs]}]
  (+ intercept (* slope (last xs))))

(defn- stability-stats
  "Compare a zero-trend model with a linear-trend model on `ys`.

  `ys` are `k` observations at `x_i = 0..k-1`. The stable model is
  `y_i = mu + e_i` and the trend model is `y_i = alpha + beta*x_i + e_i`,
  both with Gaussian errors. Returns `{:trend-slope :slope-se :t :df
  :p-value :bic-stable :bic-trend :delta-bic :likelihood :stable?}`.

  `:p-value` is the two-sided slope t-test with `k - 2` degrees of
  freedom. `:likelihood` is the BIC pseudo-posterior
  `1 / (1 + exp((BIC0 - BIC1) / 2))` under equal model priors, where
  `BIC0 = k*ln(SSE0/k) + ln(k)` and `BIC1 = k*ln(SSE1/k) + 2*ln(k)`.

  Degenerate cases: with `k < 3` there is no slope test, so the test
  and BIC keys are nil and `:stable?` is exact constancy. A constant
  series has `:likelihood 1.0`, `:p-value 1.0`, `:t 0.0` and
  `:stable? true`; the BIC keys are nil because `ln(SSE0)` is
  undefined. An exact non-flat line has `:likelihood 0.0`, `:p-value
  0.0`, `:t ##Inf` and `:stable? false`. Otherwise `:stable?` is
  `(>= likelihood 0.5)`."
  [ys]
  (let [k (count ys)
        constant? (every? #(= % (first ys)) ys)]
    (if (< k 3)
      {:trend-slope nil
       :slope-se nil
       :t nil
       :df nil
       :p-value nil
       :bic-stable nil
       :bic-trend nil
       :delta-bic nil
       :likelihood nil
       :stable? constant?}
      (let [xs (mapv double (range k))
            x-bar (stats/mean xs)
            y-bar (stats/mean ys)
            sxx (reduce + (map (fn [x]
                                 (let [d (- x x-bar)] (* d d)))
                               xs))
            sxy (reduce + (map (fn [x y]
                                 (* (- x x-bar) (- y y-bar)))
                               xs ys))
            slope (/ sxy sxx)
            intercept (- y-bar (* slope x-bar))
            sse0 (reduce + (map (fn [y]
                                  (let [d (- y y-bar)] (* d d)))
                                ys))
            sse1 (reduce + (map (fn [x y]
                                  (let [d (- y (+ intercept (* slope x)))]
                                    (* d d)))
                                xs ys))
            df (- k 2)]
        (cond
          (zero? (double sse0))
          {:trend-slope 0.0
           :slope-se 0.0
           :t 0.0
           :df df
           :p-value 1.0
           :bic-stable nil
           :bic-trend nil
           :delta-bic nil
           :likelihood 1.0
           :stable? true}

          (zero? (double sse1))
          {:trend-slope slope
           :slope-se 0.0
           :t Double/POSITIVE_INFINITY
           :df df
           :p-value 0.0
           :bic-stable (+ (* k (Math/log (/ sse0 k))) (Math/log k))
           :bic-trend Double/NEGATIVE_INFINITY
           :delta-bic Double/POSITIVE_INFINITY
           :likelihood 0.0
           :stable? false}

          :else
          (let [slope-se (Math/sqrt (/ (/ sse1 df) sxx))
                t (/ slope slope-se)
                p (stats/p-value (r/distribution :t {:degrees-of-freedom df})
                                 (Math/abs t))
                bic-stable (+ (* k (Math/log (/ sse0 k))) (Math/log k))
                bic-trend (+ (* k (Math/log (/ sse1 k))) (* 2 (Math/log k)))
                delta-bic (- bic-stable bic-trend)
                likelihood (/ 1.0 (+ 1.0 (Math/exp (/ delta-bic 2.0))))]
            {:trend-slope slope
             :slope-se slope-se
             :t t
             :df df
             :p-value p
             :bic-stable bic-stable
             :bic-trend bic-trend
             :delta-bic delta-bic
             :likelihood likelihood
             :stable? (>= likelihood 0.5)}))))))

(defn stable [vals]
  (let [ys (mapv double vals)
        m (stats/mean ys)
        constant? (every? #(= % (first ys)) ys)]
    (merge {:model :stable
            :intercept m
            :slope 0.0
            :r-squared (if constant? 1.0 0.0)
            :adj-r-squared 0.0
            :stabilized-yoy 0.0
            :stabilized m}
           (stability-stats ys))))

(defn trend-linear [vals]
  (let [{:keys [slope y-bar] :as s} (ols vals)]
    (assoc s
           :model :trend-linear
           :stabilized-yoy (/ slope y-bar)
           :stabilized (stabilized s))))

(defn trend-linear-pos [vals]
  (assoc (trend-linear (filter pos? vals)) :model :ols-linear-pos))

(defn trend-log [vals]
  (let [vals (->> vals
                  (filter pos?)
                  (map Math/log))
        {:keys [slope y-bar] :as s} (ols vals)]
    (assoc s
           :model :trend-log
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
        s (ols vals)]
    (assoc s :model :trend-asinh)))


(defn- ols-rows
  [[label vals]]
  [(assoc (trend-linear vals) :ticker label)
   (assoc (trend-linear-pos vals) :ticker label)
   (assoc (trend-log vals) :ticker label)
   (assoc (ols-asinh vals) :ticker label)
   (assoc (stable vals) :ticker label)])

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
                stable?]} (stable vals)]
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