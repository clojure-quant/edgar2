(ns edgar2.algo.cutoff
  "Percentile cuts shared by the fundamental screens.

  The cut is taken after the screen's universe is chosen, so the
  percentile is of that set.")

(def fraction-choices
  "Percentile fractions offered by the fundamental screens."
  [0.05 0.10 0.20 0.25 0.30 0.50])

(defn pct-label
  "Whole-number percentage label for fraction `p`."
  [p]
  (long (Math/round (* 100.0 (double p)))))

(defn cutoff-max-bottom
  "Largest value still in the bottom `p` fraction of xs."
  [xs p]
  (let [v (vec (sort xs))
        n (count v)
        k (max 1 (int (Math/ceil (* p n))))]
    (nth v (dec k))))

(defn cutoff-min-top
  "Smallest value still in the top `p` fraction of xs."
  [xs p]
  (let [v (vec (sort xs))
        n (count v)
        k (max 1 (int (Math/ceil (* p n))))]
    (nth v (- n k))))

(defn numeric
  [k rows]
  (keep (fn [r] (when (number? (k r)) (k r))) rows))
