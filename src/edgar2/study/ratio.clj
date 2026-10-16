(ns edgar2.study.ratio
  "Price ratio of two aligned close series.

  close-ratio divides one close by the other. index-to rescales a series
  so its first positive value is a chosen base. The nearby chart uses
  both: the lower pane is the peer close divided by the selected close,
  then indexed to 1 on the first day both closes exist.")

(set! *warn-on-reflection* true)

(defn- positive?
  [x]
  (and (number? x)
       (let [d (double x)]
         (and (Double/isFinite d) (pos? d)))))

(defn- round4
  [x]
  (-> x double (* 10000.0) Math/round double (/ 10000.0)))

(defn close-ratio
  "`num` divided by `den`, one value per index.

  Nil where either value is missing or not a positive finite number.
  The nearby chart passes the selected close as `den` and the peer
  close as `num`."
  [den num]
  (mapv (fn [a b]
          (when (and (positive? a) (positive? b))
            (/ (double b) (double a))))
        den num))

(defn index-to
  "Scale `values` so the first positive number equals `base`. Gaps stay nil."
  [values base]
  (let [origin (first (filter positive? values))
        base (double base)]
    (if-not origin
      (vec (repeat (count values) nil))
      (let [origin (double origin)]
        (mapv (fn [v]
                (when (positive? v)
                  (let [v (double v)]
                    (if (== v origin)
                      base
                      (round4 (* base (/ v origin)))))))
              values)))))
