(ns edgar2.regression.asinh
  "Asinh earnings fit: asinh(E(n)) = a + b*n.

  Unlike the log fit, non-positive earnings stay in the fit.")

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- asinh
  [x]
  (let [x (double x)
        ax (Math/abs x)
        m (Math/log (+ ax (Math/sqrt (inc (* ax ax)))))]
    (if (neg? x) (- m) m)))

(defn- error-fn
  [error]
  (case error
    :square (fn [d] (* d d))
    :absolute (fn [d] (Math/abs (double d)))
    (throw (IllegalArgumentException.
             (str "Unknown :error " error ", expected :square or :absolute")))))

(defn regression
  "OLS of asinh(earnings) on position in an oldest-first vector.

  Every number is kept, so losses and zeros stay in the fit. Nils and
  non-numbers are left out; their slots still advance n, so a missing year
  counts as a year. Needs two values at different positions, otherwise nil.

  `opts` selects how residuals are measured: `{:error :square}` (default)
  uses squared residuals, `{:error :absolute}` uses absolute residuals. The
  fitted slope and intercept are the same OLS fit either way; the choice
  affects only `:variation`.

  Returns the fit primitives `{:intercept :slope :variation :n}`, where `:n`
  is the last input index. `estimate` turns them back into earnings at an
  offset from that index, and `slope-percent` is 100 * (exp(slope) - 1). For
  large positive earnings asinh(E) = ln(2E), so this matches the log fit's
  growth; for losses it is sign-correct (shrinking losses give positive
  growth, growing losses negative). Near zero it is approximate, not a true
  percentage.

  `:variation` is 100 * (exp(s) - 1) on the asinh residuals: a relative-error
  analogue, approximate near zero. For `:square`, s = sqrt(SSE / (k - 2));
  for `:absolute`, s = SAE / (k - 2). An exact line (fewer than 3 points, or
  zero error) is 0."
  ([vals] (regression {:error :square} vals))
  ([opts vals]
   (let [err-fn (error-fn (or (:error opts) :square))
         vals (vec vals)
         points (vec (keep-indexed (fn [i v]
                                     (when (number? v)
                                       [(double i) (asinh v)]))
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
                 err (reduce + (map (fn [n y]
                                      (err-fn (- y (+ intercept (* slope n)))))
                                    ns ys))
                 variation (if (or (< k 3) (zero? (double err)))
                             0.0
                             (let [s (if (= :absolute (:error opts))
                                       (/ err (- k 2))
                                       (Math/sqrt (/ err (- k 2))))]
                               (* 100.0 (- (Math/exp s) 1.0))))]
              {:intercept intercept
               :slope slope
               :variation variation
               :n n-last})))))))

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
