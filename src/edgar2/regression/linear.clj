(ns edgar2.financials.regression.linear
  "Linear earnings fit: E(n) = a + b*n.")

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
  "OLS of earnings on position in an oldest-first vector.

  Every number is kept, so losses and zeros stay in the fit. Nils and
  non-numbers are left out; their slots still advance n, so a missing year
  counts as a year. Needs two values at different positions, otherwise nil.

  `opts` selects how residuals are measured: `{:error :square}` (default)
  uses squared residuals, `{:error :absolute}` uses absolute residuals. The
  fitted slope and intercept are the same OLS fit either way; the choice
  affects only `:earnings-variation`.

  `:earnings-stabilized-yoy` is 100 * slope / mean absolute earnings over the
  included values. This is average annual growth relative to the mean absolute
  earnings level; for a true linear series it approximates compound growth, so
  it is not the exact exponential rate the log and asinh fits report. It is 0
  when the mean absolute earnings is 0.
  `:earnings-stabilized` is the fitted earnings at the last index, the most
  recent year in the vector.
  `:earnings-variation` is the residual error as a percent of the mean
  absolute earnings level. For `:square`, it is
  100 * sqrt(SSE / (k - 2)) / mean-abs-y; for `:absolute`, it is
  100 * (SAE / (k - 2)) / mean-abs-y. An exact line (fewer than 3 points, or
  zero error) is 0, as is a zero mean absolute level."
  ([vals] (regression {:error :square} vals))
  ([opts vals]
   (let [err-fn (error-fn (or (:error opts) :square))
         vals (vec vals)
         points (vec (keep-indexed (fn [i v]
                                     (when (number? v)
                                       [(double i) (double v)]))
                                   vals))]
     (when (>= (count points) 2)
       (let [ns (mapv first points)
             ys (mapv second points)
             n-bar (mean ns)
             y-bar (mean ys)
             ss-n (reduce + (map (fn [n]
                                   (let [d (- n n-bar)]
                                     (* d d)))
                                 ns))]
         (when-not (zero? (double ss-n))
           (let [slope (/ (reduce + (map (fn [n y]
                                           (* (- n n-bar) (- y y-bar)))
                                         ns ys))
                          ss-n)
                 intercept (- y-bar (* slope n-bar))
                 n-last (double (dec (count vals)))
                 k (count ys)
                 mean-abs-y (mean (map #(Math/abs (double %)) ys))
                 err (reduce + (map (fn [n y]
                                      (err-fn (- y (+ intercept (* slope n)))))
                                    ns ys))
                 variation (if (or (< k 3) (zero? (double err))
                                   (zero? (double mean-abs-y)))
                             0.0
                             (* 100.0 (/ (if (= :absolute (:error opts))
                                           (/ err (- k 2))
                                           (Math/sqrt (/ err (- k 2))))
                                         mean-abs-y)))]
             {:earnings-stabilized-yoy (if (zero? (double mean-abs-y))
                                         0.0
                                         (* 100.0 (/ slope mean-abs-y)))
              :earnings-stabilized (+ intercept (* slope n-last))
              :earnings-variation variation})))))))

(def linear-regression
  "Alias for `regression`."
  regression)
