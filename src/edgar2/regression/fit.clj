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

(defn regression
  "Fit transformed earnings on position in an oldest-first vector.

  `vals` is the input vector; `preprocess` owns filtering and
  transformation, receiving each entry and returning the transformed value
  or nil to skip it. Skipped slots still advance the index, so a missing
  year counts as a year. Needs two transformed values at different
  positions, otherwise nil.

  `opts` selects the line and how residuals are measured: `{:error
  :square}` (default) fits by least squares, `{:error :absolute}` fits by
  least absolute deviations.

  `summarize` receives `{:error :k :err :ys}` and returns the
  method-specific fields, such as `:variation`; it must not emit
  `:intercept`, `:slope`, or `:n`. The result is those fields merged with
  `{:intercept :slope :n}`, where `:n` is the last input index."
  [opts vals preprocess summarize]
  (let [error (or (:error opts) :square)
        err-fn (error-fn error)
        vals (vec vals)
        points (vec (keep-indexed (fn [i v]
                                    (when-let [y (preprocess v)]
                                      [(double i) y]))
                                  vals))]
    (when (>= (count points) 2)
      (when-let [[intercept slope] (if (= :absolute error)
                                     (lad points)
                                     (ols points))]
        (let [n-last (double (dec (count vals)))
              k (count points)
              err (reduce + (map (fn [[n y]]
                                   (err-fn (- y (+ intercept (* slope n)))))
                                 points))
              summarized (summarize {:error error
                                     :k k
                                     :err err
                                     :ys (mapv second points)})]
          (merge {:intercept intercept :slope slope :n n-last} summarized))))))
