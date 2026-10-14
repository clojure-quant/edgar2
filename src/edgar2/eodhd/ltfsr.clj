(ns edgar2.eodhd.ltfsr
  "Longterm fractal support resistance.

  A base price is the bottom of an octave. The band runs from that base
  to twice the base: position 0.0 at the base, 10.0 at the top. A close
  above the band is placed by doubling the base, a close below it by
  halving the base. How many times the base was doubled is the octave
  (negative when it was halved).

  Closes are counted in `interval-count` equal parts of the band.
  With a base of 10 and 10 intervals those parts are the prices 10.0
  to 20.0. The search slides the base from the given base up to twice
  that base and keeps the base whose bottom and top intervals are
  thinnest against the average of the intervals between them."
  (:require [tech.v3.dataset :as ds]))

(defn position
  "Where `price` sits in the octave of `base-price`.

  The base band is [base-price, 2*base-price]. Position is 0.0 at the
  low and 10.0 at the high, so a price halfway up the band is 5.0.
  A price above the band doubles the base until it fits. A price below
  the band halves the base until it fits. The top of a band stays in
  that octave: exactly twice the base is position 10.0, not the next
  octave.

  Octave 0 is the base band. Octave 1 is one doubling. Octave -1 is one
  halving. Octave 7 is seven doublings. Octave -5 is five halvings.

  (position 10.0 12.0) => {:octave 0 :position 2.0}
  (position 10.0 14.0) => {:octave 0 :position 4.0}
  (position 10.0 18.0) => {:octave 0 :position 8.0}"
  [base-price price]
  (let [base (double base-price)
        px (double price)]
    (when-not (and (Double/isFinite base) (pos? base)
                   (Double/isFinite px) (pos? px))
      (throw (ex-info "base-price and price must be positive finite numbers"
                      {:base-price base-price :price price})))
    (loop [b base
           octave 0
           guard 0]
      (if (> guard 80)
        (throw (ex-info "price did not fall in an octave band"
                        {:base-price base-price :price price}))
        (let [hi (* 2.0 b)]
          (cond
            (<= b px hi)
            {:octave octave
             :position (* 10.0 (/ (- px b) b))}

            (> px hi)
            (recur hi (inc octave) (inc guard))

            :else
            (recur (* 0.5 b) (dec octave) (inc guard))))))))

(defn- close-column
  "Close column of a bar dataset. Rows also carry :date; the count uses closes."
  [bar-ds]
  (or (:close bar-ds)
      (throw (ex-info "bar dataset needs a :close column"
                      {:columns (vec (ds/column-names bar-ds))}))))

(defn- interval-count*
  "Interval count as a long. At least three, so the band has a bottom,
  a top, and something between them."
  [interval-count]
  (let [n (double interval-count)]
    (when-not (and (Double/isFinite n)
                   (>= n 3.0)
                   (== n (Math/rint n)))
      (throw (ex-info "interval-count must be an integer of at least 3"
                      {:interval-count interval-count})))
    (long n)))

(defn- bin-index
  "Interval index for a position in [0, 10], or nil when `price` cannot
  be placed. Position 10.0, the top of the band, belongs to the last
  interval."
  [base-price n price]
  (when (and (number? price)
             (Double/isFinite (double price))
             (pos? (double price)))
    (let [p (double (:position (position base-price price)))
          i (long (Math/floor (* p (/ (double n) 10.0))))]
      (if (>= i n)
        (dec n)
        i))))

(defn position-distribution
  "Counts of close positions in `interval-count` equal parts of the band.

  The position range 0.0–10.0 is split into that many intervals. The
  last interval includes position 10.0. With `base-price` 10.0 and 10
  intervals the parts are the prices [10, 11), [11, 12), ... [19, 20].
  Closes that are not positive finite numbers are left out. Returns a
  vector of counts, one per interval."
  [base-price interval-count bar-ds]
  (let [n (interval-count* interval-count)
        counts (long-array n)]
    (reduce (fn [_ price]
              (when-let [i (bin-index base-price n price)]
                (aset counts i (unchecked-inc (aget counts i))))
              nil)
            nil
            (close-column bar-ds))
    (vec counts)))

(defn- base-candidates
  "Bases in [base-price, 2*base-price), spaced by `step-size`.

  Twice the base is the same octave as the base, so that endpoint is
  left out."
  [base-price step-size]
  (let [base (double base-price)
        step (double step-size)
        hi (* 2.0 base)
        ;; Keep a base that float noise pushed a hair under the endpoint
        ;; out of the list. A real candidate sits a full step below it.
        limit (- hi (* step 1.0e-6))]
    (when-not (and (Double/isFinite base) (pos? base)
                   (Double/isFinite step) (pos? step))
      (throw (ex-info "base-price and step-size must be positive finite numbers"
                      {:base-price base-price :step-size step-size})))
    (loop [i 0
           out (transient [])]
      (let [b (+ base (* step (double i)))]
        (if (< b limit)
          (recur (inc i) (conj! out b))
          (persistent! out))))))

(defn interval-score
  "How thin the ends of the band are compared with the middle.

  :edges-avg is the average count of the lowest and highest intervals.
  :others-avg is the average count of every interval between them.
  :ratio is the edge average divided by the middle average. A low ratio
  means the bottom and the top of the band both hold fewer closes than
  the intervals in between."
  [counts]
  (let [n (count counts)]
    (when (< n 3)
      (throw (ex-info "interval score needs at least 3 intervals"
                      {:intervals n})))
    (let [lo (double (first counts))
          hi (double (peek counts))
          edges-avg (/ (+ lo hi) 2.0)
          mid (subvec counts 1 (dec n))
          others-avg (/ (double (reduce + mid)) (double (count mid)))]
      {:low-count (long lo)
       :high-count (long hi)
       :edges-avg edges-avg
       :others-avg others-avg
       :ratio (if (pos? others-avg) (/ edges-avg others-avg) ##Inf)})))

(defn position-search
  "Base in [base-price, 2*base-price) whose band ends are thinnest.

  Candidates are `base-price`, then `base-price` + `step-size`, and so
  on, up to but not including twice `base-price`. For a starting base
  of 10.0 that is the bases 10.0, 10.0+step, ... approaching 20.0.
  `interval-count` is how many equal parts the position range is split
  into.

  Each candidate is scored from `position-distribution`. The score is
  the average count of the lowest and highest intervals divided by the
  average count of the intervals between them. The lowest score wins:
  the bottom and the top of the band should both be scarce relative to
  the rest."
  [base-price step-size interval-count bar-ds]
  (let [n (interval-count* interval-count)
        scored (mapv (fn [b]
                       (let [dist (position-distribution b n bar-ds)
                             score (interval-score dist)]
                         (assoc score
                                :base-price (double b)
                                :interval-count n
                                :distribution dist)))
                     (base-candidates base-price step-size))]
    (when (empty? scored)
      (throw (ex-info "no base prices in the search range"
                      {:base-price base-price :step-size step-size})))
    ;; thinnest ends, then the smaller edge average, then the lower base
    (first (sort-by (juxt :ratio :edges-avg :base-price) scored))))
