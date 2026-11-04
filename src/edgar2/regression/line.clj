(ns edgar2.regression.line
  "Univariate line fits on transformed points `[[x y] ...]`.")

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
