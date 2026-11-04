(ns edgar2.regression.util)

(defn point-map
  "Build `[[index transformed] ...]` from `vals`, keeping only entries where
  `preprocess-one` returns non-nil. Skipped slots still advance the index."
  [vals preprocess-one]
  (vec (keep-indexed (fn [i v]
                       (when-let [y (preprocess-one v)]
                         [(double i) y]))
                     vals)))
