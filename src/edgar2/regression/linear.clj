(ns edgar2.regression.linear
  "Linear earnings fit: E(n) = a + b*n."
  (:require [edgar2.regression.line :as line]))

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- error-fn
  [error]
  (case error
    :square (fn [d] (* d d))
    :absolute (fn [d] (Math/abs (double d)))
    (throw (IllegalArgumentException.
             (str "Unknown :error " error ", expected :square or :absolute")))))

(defn regression
  "Fit earnings on position in an oldest-first vector.

  Every number is kept, so losses and zeros stay in the fit. Nils and
  non-numbers are left out; their slots still advance n, so a missing year
  counts as a year. Needs two values at different positions, otherwise nil.

  `opts` selects the line and how residuals are measured: `{:error
  :square}` (default) fits by least squares, `{:error :absolute}` fits by
  least absolute deviations. The choice affects the line and `:variation`.

  Returns the fit primitives
  `{:intercept :slope :variation :n :mean}`, where `:n` is the last input
  index and `:mean` is the mean absolute earnings over the included values.
  `estimate` turns them back into earnings at an offset from that index, and
  `slope-percent` is 100 * slope / `:mean`. That is average annual growth
  relative to the mean absolute earnings level; for a true linear series it
  approximates compound growth, so it is not the exact exponential rate the
  log and asinh fits report. It is 0 when the mean absolute earnings is 0.

  `:variation` is the residual error as a percent of the mean absolute
  earnings level. For `:square`, it is
  100 * sqrt(SSE / (k - 2)) / mean-abs-y; for `:absolute`, it is
  100 * (SAE / (k - 2)) / mean-abs-y. An exact line (fewer than 3 points, or
  zero error) is 0, as is a zero mean absolute level."
  ([vals] (regression {:error :square} vals))
  ([opts vals]
   (let [error (or (:error opts) :square)
         err-fn (error-fn error)
         vals (vec vals)
         points (vec (keep-indexed (fn [i v]
                                     (when (number? v)
                                       [(double i) (double v)]))
                                   vals))]
     (when (>= (count points) 2)
       (when-let [[intercept slope] (if (= :absolute error)
                                      (line/lad points)
                                      (line/ols points))]
         (let [n-last (double (dec (count vals)))
               k (count points)
               mean-abs-y (mean (map #(Math/abs (double %)) (map second points)))
               err (reduce + (map (fn [[n y]]
                                    (err-fn (- y (+ intercept (* slope n)))))
                                  points))
               variation (if (or (< k 3) (zero? (double err))
                                 (zero? (double mean-abs-y)))
                           0.0
                           (* 100.0 (/ (if (= :absolute error)
                                         (/ err (- k 2))
                                         (Math/sqrt (/ err (- k 2))))
                                       mean-abs-y)))]
           {:intercept intercept
            :slope slope
            :variation variation
            :n n-last
            :mean mean-abs-y}))))))

(defn estimate
  "Fitted earnings at `offset` periods past the last input index.

  Offset 0 is the current fitted value, 1 is one period ahead. Nil when
  `regression` is nil."
  [regression offset]
  (when regression
    (let [{:keys [intercept slope n]} regression]
      (+ intercept (* slope (+ n offset))))))

(defn slope-percent
  "Fitted growth as a percent of mean absolute earnings.

  Zero when the mean absolute earnings is zero. Nil when `regression` is nil."
  [regression]
  (when regression
    (let [m (double (:mean regression))]
      (if (zero? m)
        0.0
        (* 100.0 (/ (:slope regression) m))))))
