(ns edgar2.regression.log
  "Log-linear earnings fit: E(n) = E(0) * (1 + g)^n."
  (:require [edgar2.regression.fit :as fit]
            [edgar2.regression.util :as util]))

(defn- preprocess-value
  [v]
  (when (and (number? v) (pos? (double v)))
    (Math/log (double v))))

(defn preprocess
  "Transform `vals` into `[[index transformed] ...]`, skipping entries
  `preprocess-value` rejects."
  [vals]
  (util/point-map vals preprocess-value))

(defn- err-normalize ; log
  [{:keys [e]}]
  (* 100.0 (- (Math/exp e) 1.0)))

(defn regression
  "Fit ln(earnings) on position in an oldest-first vector.

  Non-positive entries (negatives, zeros, nils) are left out of the fit.
  Their slots still advance n, so a dropped year counts as a year. Needs
  two positive values at different positions, otherwise nil.

  `opts` selects the line and how residuals are measured: `{:error
  :square}` (default) fits by least squares, `{:error :absolute}` fits by
  least absolute deviations. The choice affects the line and `:variation`.

  Returns the fit primitives `{:intercept :slope :variation :n}`, where `:n`
  is the last input index. `estimate` turns them back into earnings at an
  offset from that index, and `slope-percent` is 100 * g, with
  g = exp(slope) - 1.

  `:variation` is the residual error as a percent: 100 * (exp(s) - 1). For
  `:square`, s = sqrt(SSE / (k - 2)) on the log residuals of the positive
  years. For `:absolute`, s = sqrt(2 * SAE / (k - 2)), the Laplace
  standard-deviation-equivalent. An exact line (fewer than 3 points, or zero
  error) is 0. A value of 20 means a typical year sits about 20% off the
  path."
  ([vals] (regression {:error :square} vals))
  ([opts vals]
   (fit/regression opts vals preprocess err-normalize)))

(defn estimate
  "Fitted earnings at `offset` periods past the last input index.

  Offset 0 is the current fitted value, 1 is one period ahead. Nil when
  `regression` is nil."
  [regression offset]
  (when regression
    (let [{:keys [intercept slope n]} regression]
      (Math/exp (+ intercept (* slope (+ n offset)))))))

(defn slope-percent
  "Fitted growth as a percent: 100 * (exp(slope) - 1). Nil when nil."
  [regression]
  (when regression
    (* 100.0 (- (Math/exp (:slope regression)) 1.0))))
