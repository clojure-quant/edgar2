(ns demo.net-income-model
  "Classify each cached net-income series as stable or trending.

  Reads `data/ni.edn` (written by `demo.net-income/run`), classifies each
  `:ni` series with `edgar2.regression.net-income-model/classify-series`
  (which delegates to `demo.regression-fastmath/stable` and, when the
  series is not stable, the linear trend model). Writes the resulting
  models to `data/ni-trend.edn`.

  Run with: clj -X:demo-net-income-model"
  (:require [edgar2.download.dataset :as dataset]
            [edgar2.regression.net-income-model :as nim])
  (:import [java.time LocalDate]))

(def ni-path "data/ni.edn")
(def out-path "data/ni-trend.edn")

(def ^:private min-observations
  "Tickers with fewer valid net-income observations are dropped."
  5)

(defn- classify-row
  "Model map for one `data/ni.edn` row, tagged with :ticker. Nil when the
  row has no ticker or fewer than `min-observations` net-income values."
  [{:keys [ticker ni]}]
  (let [vals (remove nil? ni)]
    (when (and (some? ticker) (>= (count vals) min-observations))
      (when-let [model (nim/classify-series vals)]
        (assoc model :ticker ticker)))))

(defn classify-ticker
  "Load `ni-path` and classify the row for `ticker`. Nil when not found.

  Convenience for interactive use; `run` classifies rows directly to avoid
  re-reading the file per ticker."
  [ticker]
  (some (fn [row] (when (= ticker (:ticker row)) (classify-row row)))
        (dataset/read-rows ni-path)))

(defn- trend-quintile-bounds
  "Value bounds `[lo hi]` of each equal-count quintile, low to high.

  `ys` are sorted and each rank `i` of `n` is assigned to
  `(min 4 (quot (* i 5) n))`, giving five roughly equal groups. Returns a
  vector of five `[lo hi]` pairs; all nil when `ys` is empty."
  [ys]
  (let [ys (->> ys (filter number?) (sort) vec)
        n (count ys)]
    (if (zero? n)
      [nil nil nil nil nil]
      (let [groups (group-by (fn [i] (min 4 (quot (* i 5) n)))
                             (range n))]
        (mapv (fn [q]
                (when-let [idxs (seq (get groups q))]
                  [(ys (first idxs)) (ys (last idxs))]))
              (range 5))))))

(defn run
  "Classify every series in `ni-path` into `out-path`."
  ([] (run {}))
  ([_]
   (let [in (dataset/read-rows ni-path)]
     (assert (seq in) (str "No rows in " ni-path))
     (let [rows (->> in (keep classify-row) vec)
           by-model (group-by :model rows)
           trend-rows (:trend-linear by-model)
           significant (filter nim/trend-significant? trend-rows)
           n-stable (count (:stable by-model))
           n-trend (count trend-rows)
           quintiles (trend-quintile-bounds (map :stabilized-yoy trend-rows))
           significant-quintiles (trend-quintile-bounds
                                  (map :stabilized-yoy significant))]
       (dataset/write-dataset! out-path
                               {:download-date (str (LocalDate/now))
                                :n-stable n-stable
                                :n-trend n-trend
                                :n-trend-significant (count significant)
                                :trend-quintiles quintiles
                                :trend-significant-quintiles significant-quintiles}
                               rows)
       (println (format "Wrote %s  (%d tickers: %d stable, %d trend, %d significant)"
                        out-path (count rows) n-stable n-trend (count significant)))
       (println (format "Trend quintile bounds (low to high): %s" quintiles))
       (println (format "Significant quintile bounds (low to high): %s"
                        significant-quintiles))
       rows))))

(comment
  (run)
  (nim/classify-series [5 5 5 5 5])
  (nim/classify-series [1 2 3 4 5])
  (classify-ticker "AIR")
  ;
  )
