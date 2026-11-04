(ns edgar2.regression.fit
  "Univariate line fits and the shared earnings-regression pipeline.")

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn ols
  "Least-squares line through `points` as `[intercept slope]`.

  Nil when every x is equal."
  [points]
  (let [xs (mapv first points)
        ys (mapv second points)
        x-bar (mean xs)
        y-bar (mean ys)
        ss-x (reduce + (map (fn [x]
                              (let [d (- x x-bar)]
                                (* d d)))
                            xs))]
    (when-not (zero? (double ss-x))
      (let [slope (/ (reduce + (map (fn [x y]
                                      (* (- x x-bar) (- y y-bar)))
                                    xs ys))
                     ss-x)]
        [(- y-bar (* slope x-bar)) slope]))))

(defn- absolute-error
  [intercept slope points]
  (reduce + (map (fn [[x y]]
                   (Math/abs (double (- y (+ intercept (* slope x))))))
                 points)))

(defn lad
  "Least-absolute-deviations line through `points` as `[intercept slope]`.

  An L1 optimum passes through two of the points, so scoring every pair
  finds it. Nil when there are fewer than two points with distinct x."
  [points]
  (let [pairs (for [i (range (count points))
                    j (range (inc i) (count points))
                    :let [[[x1 y1] [x2 y2]] [(nth points i) (nth points j)]]
                    :when (not= x1 x2)]
                (let [slope (/ (- y2 y1) (- x2 x1))]
                  [(- y1 (* slope x1)) slope]))]
    (when (seq pairs)
      (apply min-key (fn [[a b]] (absolute-error a b points)) pairs))))


(defn- error-fn
  [error]
  (case error
    :square (fn [d] (* d d))
    :absolute (fn [d] (Math/abs (double d)))
    (throw (IllegalArgumentException.
            (str "Unknown :error " error ", expected :square or :absolute")))))

(defn err
  "Residual standard-deviation-equivalent of the fit, on the scale of the
  residuals.

  For `:square` this is sqrt(SSE / (k - 2)). For `:absolute` it is the
  Laplace sigma-equivalent sqrt(2 * SAE / (k - 2)), since a Laplace error
  with mean absolute deviation b has standard deviation sqrt(2) * b. Both
  are 0 when there are at most two points."
  [intercept slope points error]
  (let [k (count points)
        err-fn (error-fn error)
        err-sum (reduce + (map (fn [[n y]]
                                 (err-fn (- y (+ intercept (* slope n)))))
                               points))
        err (if (> k 2)
              (if (= :absolute error)
                (Math/sqrt (* 2.0 (/ err-sum (- k 2))))
                (Math/sqrt (/ err-sum (- k 2))))
              0.0)]
    err))


(defn regression
  "Fit transformed earnings on position in an oldest-first vector.

  `vals` is the input vector; `preprocess` owns filtering and
  transformation, receiving the whole vector and returning
  `[[index transformed] ...]` with rejected entries already dropped.
  Skipped slots still advance the index, so a missing year counts as a
  year. Needs two transformed values at different positions, otherwise
  nil.

  `opts` selects the line and how residuals are measured: `{:error
  :square}` (default) fits by least squares, `{:error :absolute}` fits by
  least absolute deviations.

  `err-normalize` receives the fit result `{:intercept :slope :n :points
  :error :e}` and returns the method-specific `:variation`. The result is
  that map with `:variation` added, where `:n` is the last input index.

  `:e` is the residual standard-deviation-equivalent from `err`, so both
  error modes report on the same scale."
  [opts vals preprocess err-normalize]
  (let [error (or (:error opts) :square)
        vals (vec vals)
        points (preprocess vals)]
    (when (>= (count points) 2)
      (when-let [[intercept slope] (if (= :absolute error)
                                     (lad points)
                                     (ols points))]
        (let [n-last (double (dec (count vals)))
              e (err intercept slope points error)
              result {; regression output
                      :intercept intercept
                      :slope slope
                                ; for current stabilized value
                      :n n-last
                                ; for error estimate
                      :points points
                      :error error
                      :e e}]
          (assoc result :e e :variation (err-normalize result))
          )))))





