(ns edgar2.regression.asinh
  "Asinh earnings fit: asinh(E(n)) = a + b*n.

  Unlike the log fit, non-positive earnings stay in the fit."
  (:require [edgar2.regression.fit :as fit]))

(defn- asinh
  [x]
  (let [x (double x)
        ax (Math/abs x)
        m (Math/log (+ ax (Math/sqrt (inc (* ax ax)))))]
    (if (neg? x) (- m) m)))

(defn- preprocess
  [v]
  (when (number? v)
    (asinh v)))

(defn- err-normalize ; asinh
  [{:keys [e]}]
  (* 100.0 (- (Math/exp e) 1.0)))

(defn regression
  "Fit asinh(earnings) on position in an oldest-first vector.

  Every number is kept, so losses and zeros stay in the fit. Nils and
  non-numbers are left out; their slots still advance n, so a missing year
  counts as a year. Needs two values at different positions, otherwise nil.

  `opts` selects the line and how residuals are measured: `{:error
  :square}` (default) fits by least squares, `{:error :absolute}` fits by
  least absolute deviations. The choice affects the line and `:variation`.

  Returns the fit primitives `{:intercept :slope :variation :n}`, where `:n`
  is the last input index. `estimate` turns them back into earnings at an
  offset from that index, and `slope-percent` is 100 * (exp(slope) - 1). For
  large positive earnings asinh(E) = ln(2E), so this matches the log fit's
  growth; for losses it is sign-correct (shrinking losses give positive
  growth, growing losses negative). Near zero it is approximate, not a true
  percentage.

  `:variation` is 100 * (exp(s) - 1) on the asinh residuals: a relative-error
  analogue, approximate near zero. For `:square`, s = sqrt(SSE / (k - 2));
  for `:absolute`, s = sqrt(2 * SAE / (k - 2)), the Laplace
  standard-deviation-equivalent. An exact line (fewer than 3 points, or zero
  error) is 0."
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
      (Math/sinh (+ intercept (* slope (+ n offset)))))))

(defn slope-percent
  "Fitted growth as a percent: 100 * (exp(slope) - 1). Nil when nil."
  [regression]
  (when regression
    (* 100.0 (- (Math/exp (:slope regression)) 1.0))))
