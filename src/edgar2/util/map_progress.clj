(ns edgar2.util.map-progress
  "Eager maps that print a progress line every 500 items.")

(def ^:private progress-every 500)

(defn- report-progress?
  "True on every 500th 1-based index and on the last item."
  [i total]
  (or (zero? (mod i progress-every))
      (= i total)))

(defn- run-indexed
  "Apply `transform-fn` to one indexed input and maybe print progress.
  `indexed` is `[i input-data]` with `i` 0-based. `progress-fn` is called
  with the 1-based index, `total`, and that item's result when the index
  is a multiple of 500 or is the last item. A string returned by
  `progress-fn` is printed. Returns the result."
  [progress-fn transform-fn total [i input-data]]
  (let [result (transform-fn input-data)
        i' (inc i)]
    (when (report-progress? i' total)
      (when-let [msg (progress-fn i' total result)]
        (when (string? msg)
          (println msg)
          (flush))))
    result))

(defn map-progress
  "Apply `transform-fn` to each item of `items`. After every 500th item
  and after the last item, `progress-fn` is called with the 1-based
  index, the total count, and that item's result. A string returned by
  `progress-fn` is printed.
  Returns a vector of the results. The walk is eager."
  [progress-fn transform-fn items]
  (let [items (vec items)
        total (count items)]
    (loop [i 0
           acc (transient [])]
      (if (< i total)
        (let [result (run-indexed progress-fn transform-fn total [i (nth items i)])]
          (recur (inc i) (conj! acc result)))
        (persistent! acc)))))

(defn pmap-progress
  "Like `map-progress`, but `transform-fn` runs in parallel via `pmap`.
  `progress-fn` runs when a finished item's 1-based input index is a
  multiple of 500 or is the last index, so lines can appear out of input
  order. The returned vector stays in input order."
  [progress-fn transform-fn items]
  (let [items (vec items)
        total (count items)
        run (bound-fn [indexed]
              (run-indexed progress-fn transform-fn total indexed))]
    (vec (pmap run (map-indexed vector items)))))
