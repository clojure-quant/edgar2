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

  `:earnings-growth-avg-yoy` is 100 * g, with g = exp(slope) - 1.
  `:earnings-stabilized` is the fitted earnings at the last index, the
  most recent year in the vector."
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
                n-last (double (dec (count vals)))]
            {:earnings-growth-avg-yoy (* 100.0 (- (Math/exp slope) 1.0))
             :earnings-stabilized (Math/exp (+ intercept (* slope n-last)))}))))))
