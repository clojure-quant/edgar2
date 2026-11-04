(ns edgar2.regression.linear
  "Linear earnings fit: E(n) = a + b*n."
  (:require [edgar2.regression.fit :as fit]))

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- preprocess
  [v]
  (when (number? v)
    (double v)))

(defn- summarize
  [{:keys [error k err ys]}]
  (let [mean-abs-y (mean (map #(Math/abs (double %)) ys))
        variation (if (or (< k 3) (zero? (double err))
                          (zero? (double mean-abs-y)))
                    0.0
                    (* 100.0 (/ (if (= :absolute error)
                                  (/ err (- k 2))
                                  (Math/sqrt (/ err (- k 2))))
                                mean-abs-y)))]
    {:variation variation}))

(defn regression
  "Fit earnings on position in an oldest-first vector.

  Every number is kept, so losses and zeros stay in the fit. Nils and
  non-numbers are left out; their slots still advance n, so a missing year
  counts as a year. Needs two values at different positions, otherwise nil.

  `opts` selects the line and how residuals are measured: `{:error
  :square}` (default) fits by least squares, `{:error :absolute}` fits by
  least absolute deviations. The choice affects the line and `:variation`.

  Returns the fit primitives `{:intercept :slope :variation :n}`, where `:n`
  is the last input index. `estimate` turns them back into earnings at an
  offset from that index, and `slope-percent` is
  100 * slope / (estimate regression 0). That is average annual growth
  relative to the current fitted earnings level; for a true linear series it
  approximates compound growth, so it is not the exact exponential rate the
  log and asinh fits report. It is 0 when the current fitted value is 0.

  `:variation` is the residual error as a percent of the mean absolute
  earnings level, which is used only as an internal scale. For `:square`, it
  is 100 * sqrt(SSE / (k - 2)) / mean-abs-y; for `:absolute`, it is
  100 * (SAE / (k - 2)) / mean-abs-y. An exact line (fewer than 3 points, or
  zero error) is 0, as is a zero mean absolute level."
  ([vals] (regression {:error :square} vals))
  ([opts vals]
   (fit/regression opts vals preprocess summarize)))

(defn estimate
  "Fitted earnings at `offset` periods past the last input index.

  Offset 0 is the current fitted value, 1 is one period ahead. Nil when
  `regression` is nil."
  [regression offset]
  (when regression
    (let [{:keys [intercept slope n]} regression]
      (+ intercept (* slope (+ n offset))))))

(defn slope-percent
  "Fitted growth as a percent of current fitted earnings.

  The current fitted value is `(estimate regression 0)`. Zero when that value
  is zero. Nil when `regression` is nil."
  [regression]
  (when regression
    (let [current (double (estimate regression 0))]
      (if (zero? current)
        0.0
        (* 100.0 (/ (:slope regression) current))))))
