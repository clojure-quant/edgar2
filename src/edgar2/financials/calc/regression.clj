(ns edgar2.financials.calc.regression
  "Log-linear earnings fit: E(n) = E(0) * (1 + g)^n.")

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- positive?
  [v]
  (and (number? v) (pos? (double v))))

(defn earnings-regression
  "OLS of ln(earnings) on position in an oldest-first vector.

  Non-positive entries (negatives, zeros, nils) are left out of the fit.
  Their slots still advance n, so a dropped year counts as a year. Needs
  two positive values at different positions, otherwise nil.

  `:earnings-stabilized-yoy` is 100 * g, with g = exp(slope) - 1.
  `:earnings-stabilized` is the fitted earnings at the last index, the
  most recent year in the vector.
  `:earnings-variation` is the residual standard error as a percent:
  100 * (exp(s) - 1), with s = sqrt(SSE / (k - 2)) on the log residuals
  of the positive years. An exact line (fewer than 3 points, or SSE of 0)
  is 0. A value of 20 means a typical year sits about 20% off the path."
  [vals]
  (let [vals (vec vals)
        points (vec (keep-indexed (fn [i v]
                                    (when (positive? v)
                                      [(double i) (Math/log (double v))]))
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
                sse (reduce + (map (fn [n y]
                                     (let [d (- y (+ intercept (* slope n)))]
                                       (* d d)))
                                   ns ys))
                variation (if (or (< k 3) (zero? (double sse)))
                            0.0
                            (let [s (Math/sqrt (/ sse (- k 2)))]
                              (* 100.0 (- (Math/exp s) 1.0))))]
            {:earnings-stabilized-yoy (* 100.0 (- (Math/exp slope) 1.0))
             :earnings-stabilized (Math/exp (+ intercept (* slope n-last)))
             :earnings-variation variation}))))))
