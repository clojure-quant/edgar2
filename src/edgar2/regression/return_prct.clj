(ns edgar2.regression.return-prct
  "Return-based price fit: r(n) = (p(n) - p(n-1)) / p(n-1).

  Summarizes adjacent percentage returns and reconstructs a fitted price
  path from the first usable price by accumulating the average return.

  There is no time-slope fit. For `:error :square` the return series is
  summarized by its mean and standard deviation; for `:error :absolute` by
  its median and mean absolute deviation from the median.")

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- median
  [xs]
  (let [xs (vec (sort xs))
        k (count xs)
        mid (quot k 2)]
    (if (odd? k)
      (double (nth xs mid))
      (/ (+ (double (nth xs (dec mid))) (double (nth xs mid))) 2.0))))

(defn- standard-deviation
  "Sample standard deviation of `xs`. 0 for fewer than two values."
  [xs]
  (let [k (count xs)]
    (if (< k 2)
      0.0
      (let [m (mean xs)]
        (Math/sqrt (/ (reduce + (map (fn [x]
                                       (let [d (- (double x) m)]
                                         (* d d)))
                                     xs))
                      (dec k)))))))

(defn- mean-absolute-deviation
  [xs]
  (let [m (median xs)]
    (mean (map (fn [x] (Math/abs (- (double x) m))) xs))))

(defn- return-at
  [prev cur]
  (when (and (number? prev) (number? cur) (pos? (double prev)))
    (/ (- (double cur) (double prev)) (double prev))))

(defn preprocess
  "Transform `vals` into `[[index return] ...]` for each adjacent pair with a
  positive numeric previous value and a numeric current value. Returns are
  fractions, e.g. 0.05 for a 5% gain. Skipped slots still advance the index."
  [vals]
  (let [vals (vec vals)]
    (vec (keep-indexed (fn [i v]
                         (when (pos? i)
                           (when-let [r (return-at (nth vals (dec i)) v)]
                             [(double i) r])))
                       vals))))

(defn base
  "Anchor for `estimate`: the previous slot of the first computable return,
  as `{:index :value}`. Nil when no return is computable."
  [vals]
  (let [vals (vec vals)]
    (some (fn [i]
            (when (pos? i)
              (let [prev (nth vals (dec i))]
                (when (return-at prev (nth vals i))
                  {:index (dec i) :value (double prev)}))))
          (range (count vals)))))




(defn- error-fn
  [error]
  (case error
    :square (fn [xs] {:slope (mean xs)
                      :e (standard-deviation xs)})
    :absolute (fn [xs] {:slope (median xs)
                        :e (mean-absolute-deviation xs)})
    (throw (IllegalArgumentException.
            (str "Unknown :error " error ", expected :square or :absolute")))))

(base [10.0 15.0 30.0 15.0])

(let [f (error-fn :square)]
  (->> [-1293.0 4467.0 2475.0 5764.0 8130.0 11264.0 6947.0]
       (preprocess)
     (mapv second)
     f
       ))
;;=> {:slope 0.25913206309242637, :e 0.7233664217368604}


(defn regression
  "Summarize adjacent percentage returns from an oldest-first price vector.

  A return is computed only when the previous value is a positive number and
  the current value is a number. Zero, negative, nil, and non-numeric
  previous values break the chain and are skipped; their slots still advance
  the index. Needs at least two returns, otherwise nil.

  `opts` selects how the return series is summarized: `{:error :square}`
  (default) uses the mean return as `:slope` and its sample standard
  deviation as `:e` and `:variation`; `{:error :absolute}` uses the median
  return as `:slope` and the mean absolute deviation from the median as
  `:e` and `:variation`.

  Returns `{:intercept :slope :n :points :error :e :variation :base-index
  :base-value}`. `:intercept` is 0.0 because there is no time term, `:n` is
  the count of returns, and `:base-index`/`:base-value` are the input index
  and price of the first usable price. `estimate` accumulates the mean
  return from that price, and `slope-percent` is 100 * slope."
  ([vals] (regression {:error :square} vals))
  ([opts vals]
   (let [error (or (:error opts) :square)
         describe (error-fn error)
         vals (vec vals)
         points (preprocess vals)]
     (when (>= (count points) 2)
       (when-let [{:keys [index value]} (base vals)]
         (let [returns (mapv second points)
               {:keys [slope e]} (describe returns)]
           {:intercept 0.0
            :slope slope
            :n (double (count returns))
            :points points
            :error error
            :variation e
            :base-index (double index)
            :base-value value}))))))

(defn estimate
  "Fitted price at `offset` periods past the last input index.

  Offset 0 is the current fitted value, 1 is one period ahead. The estimate
  accumulates the average return, not compounding:
  `base-value * (1 + slope * (n + offset))`. Nil when `regression` is nil or
  lacks the anchor."
  [regression offset]
  (when regression
    (let [{:keys [slope n base-value]} regression]
      (when (and (number? base-value) (number? n))
        (* base-value (+ 1.0 (* slope (+ n offset))))))))

(defn slope-percent
  "Average per-period return as a percent: 100 * slope. Nil when
  `regression` is nil."
  [regression]
  (when regression
    (* 100.0 (:slope regression))))
