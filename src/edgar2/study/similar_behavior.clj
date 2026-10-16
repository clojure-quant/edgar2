(ns edgar2.study.similar-behavior
  "Pairs that move together, scored so different pairs can be compared.

  nearby.edn keeps the most correlated names. This study keeps every
  unordered pair whose correlation is above 0.7 and measures the price
  ratio on the overlapping split-adjusted closes.

  The raw ratio of two share prices is not comparable: BP/TTE sits near
  2 while another pair may sit near 0.3, and a slow re-rating over
  decades stretches the band even when the two charts still track.
  Each ratio is therefore divided by its trailing one-year geometric
  mean, so 1 means \"at its usual relative price\" for every pair.
  BP and TTE are the example. Their correlation is 0.8. Around that
  trailing mean the ratio usually stays between about 0.9 and 1.1 for
  the whole overlap, and it keeps making swings of 10% that finish in
  about a month.

  :low and :high are the 5th and 95th percentiles of that centered
  ratio, widened so swapping the two tickers does not change them.
  :span is high/low. :deviations is how many of those 10% swings
  finish per year. :months is the median length of a swing, with a
  month counted as 21 sessions.

  Usage: clj -X:similar-behavior"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.eodhd.ingress :as ingress]
            [edgar2.study.ratio :as ratio]
            [tablecloth.api :as tc]
            [taoensso.timbre :as timbre])
  (:import [java.time Instant LocalDate ZoneId]
           [java.time.temporal ChronoUnit]
           [java.util Arrays]
           [java.util.concurrent Callable Executors]))

(set! *warn-on-reflection* true)

(def min-correlation
  "Pairs at or below this correlation are left out."
  0.7)

(def deviation
  "A swing counts once the ratio has moved this far from its extreme."
  0.10)

(def center-sessions
  "Trailing sessions in the geometric mean that centers the ratio.
  252 is about one year of daily bars."
  252)

(def sessions-per-month
  "Trading sessions counted as one month."
  21.0)

(def nearby-path "data/nearby.edn")
(def output-path "data/similar-behavior.edn")

(def ^:private ny (ZoneId/of "America/New_York"))

(defn- session-date
  ^LocalDate [dt]
  (cond
    (instance? LocalDate dt) dt
    (instance? Instant dt) (LocalDate/ofInstant ^Instant dt ny)
    :else (throw (ex-info "bar date is not a session" {:date dt}))))

(defn- round4
  ^double [x]
  (/ (double (Math/round (* 10000.0 (double x)))) 10000.0))

(defn- years-between
  ^double [^LocalDate start ^LocalDate end]
  (/ (double (.until start end ChronoUnit/DAYS)) 365.25))

(defn- quiet-logs!
  []
  (timbre/set-min-level! :warn))

(defn high-pairs
  "Map of [ticker ticker] to correlation for every unordered pair in
  `nearby` above min-correlation. Tickers are in alphabetical order.
  The correlation is the higher of the two directions."
  [nearby]
  (reduce
   (fn [acc [ticker peers]]
     (reduce
      (fn [acc peer]
        (let [other (:ticker peer)
              c (:correlation peer)]
          (if-not (and (string? ticker)
                       (string? other)
                       (not= ticker other)
                       (number? c)
                       (> (double c) (double min-correlation)))
            acc
            (let [[a b] (if (neg? (compare ticker other))
                          [ticker other]
                          [other ticker])]
              (update acc [a b] #(max (double c) (double (or % 0.0))))))))
      acc
      peers))
   {}
   nearby))

(defn- pack
  "Sorted session dates and positive closes for one bar dataset."
  [ds]
  (let [date-col (:date ds)
        close-col (:close ds)
        n (count date-col)
        rows (loop [i 0
                    acc (transient [])]
               (if (>= i n)
                 (persistent! acc)
                 (let [c (nth close-col i)]
                   (recur (inc i)
                          (if (and (number? c)
                                   (Double/isFinite (double c))
                                   (pos? (double c)))
                            (conj! acc [(session-date (nth date-col i))
                                        (double c)])
                            acc)))))
        rows (vec (sort-by first rows))
        m (count rows)
        dates (object-array m)
        closes (double-array m)]
    (dotimes [i m]
      (let [[d c] (nth rows i)]
        (aset dates i d)
        (aset ^doubles closes i (double c))))
    {:dates dates :closes closes :n m}))

(defn- load-one
  [ticker]
  (try
    (when-let [ds (ingress/adjusted-bars ticker)]
      (when (pos? (tc/row-count ds))
        (let [packed (pack ds)]
          (when (pos? (long (:n packed)))
            [ticker packed]))))
    (catch Exception ex
      (println (format "skip %s: %s" ticker (ex-message ex)))
      (flush)
      nil)))

(defn- parallel-map
  [f xs]
  (let [xs (vec xs)
        n (count xs)]
    (if (<= n 1)
      (mapv f xs)
      (let [threads (min n (max 4 (.. Runtime getRuntime availableProcessors)))
            pool (Executors/newFixedThreadPool threads)
            loader (.getContextClassLoader (Thread/currentThread))]
        (try
          (into []
                (map deref)
                (.invokeAll pool
                            (mapv (fn [x]
                                    (reify Callable
                                      (call [_]
                                        (.setContextClassLoader (Thread/currentThread) loader)
                                        (f x))))
                                  xs)))
          (finally
            (.shutdown pool)))))))

(defn- load-closes
  "Map of ticker to packed closes. Tickers that fail to load are absent."
  [tickers]
  (let [tickers (vec (sort (set tickers)))
        n (count tickers)
        done (atom 0)
        loaded (parallel-map
                (fn [ticker]
                  (let [row (load-one ticker)
                        i (swap! done inc)]
                    (when (or (= i n) (zero? (mod i 50)))
                      (println (format "loaded %d/%d" i n))
                      (flush))
                    row))
                tickers)]
    (into {} (remove nil?) loaded)))

(defn- common-ratio
  "close(a) / close(b) on sessions present in both packed series.

  Returns {:ratio :start :end :n}, or nil when the overlap is empty.
  :ratio is a double array, oldest first."
  [a b]
  (let [da ^objects (:dates a)
        db ^objects (:dates b)
        ca ^doubles (:closes a)
        cb ^doubles (:closes b)
        na (long (:n a))
        nb (long (:n b))]
    (loop [i 0
           j 0
           num (transient [])
           den (transient [])
           start nil
           end nil]
      (if (or (>= i na) (>= j nb))
        (let [num (persistent! num)
              den (persistent! den)
              values (ratio/close-ratio den num)]
          (when (seq values)
            {:ratio (double-array values)
             :start start
             :end end
             :n (count values)}))
        (let [^LocalDate x (aget da i)
              ^LocalDate y (aget db j)
              cmp (.compareTo x y)]
          (cond
            (neg? cmp) (recur (inc i) j num den start end)
            (pos? cmp) (recur i (inc j) num den start end)
            :else (recur (inc i)
                         (inc j)
                         (conj! num (aget ca i))
                         (conj! den (aget cb j))
                         (or start x)
                         x)))))))

(defn- centered
  "Ratio divided by its trailing geometric mean of `window` sessions.

  The first full window is the first returned point. Nil when `ratio`
  is shorter than `window`."
  ^doubles [^doubles ratio window]
  (let [n (alength ratio)
        w (long window)]
    (when (>= n w)
      (let [out (double-array (inc (- n w)))
            sum-box (double-array 1)]
        (dotimes [i n]
          (let [log-v (Math/log (aget ratio i))
                sum (+ (aget sum-box 0) log-v)
                sum (if (>= i w)
                      (- sum (Math/log (aget ratio (- i w))))
                      sum)]
            (aset sum-box 0 sum)
            (when (>= i (dec w))
              (aset out (- i (dec w))
                    (/ (aget ratio i) (Math/exp (/ sum (double w))))))))
        out))))

(defn- percentile
  ^double [^doubles xs p]
  (let [n (alength xs)
        copy (aclone xs)
        i (-> (* (double p) (dec n))
              Math/round
              long
              (max 0)
              (min (dec n)))]
    (Arrays/sort copy)
    (aget copy i)))

(defn- band
  "Order-invariant 5th and 95th percentile of a centered ratio.

  Inverting the ratio swaps the two percentiles around 1. The returned
  low is the smaller of those four numbers and high is the larger, so
  BP/TTE and TTE/BP report the same band."
  [^doubles centered-ratio]
  (let [lo (percentile centered-ratio 0.05)
        hi (percentile centered-ratio 0.95)]
    (when (and (pos? lo) (pos? hi)
               (Double/isFinite lo) (Double/isFinite hi))
      (let [low (min lo (/ 1.0 hi))
            high (max hi (/ 1.0 lo))]
        (when (and (pos? low) (>= high low))
          {:low low :high high :span (/ high low)})))))

(defn- swing-sessions
  "Session counts of completed moves of at least `pct` in `ratio`.

  The first move only chooses a direction. Each later reversal of
  `pct` from the running extreme closes one swing."
  [^doubles ratio pct]
  (let [n (alength ratio)
        up-mult (+ 1.0 (double pct))
        dn-mult (/ 1.0 up-mult)]
    (loop [i 1
           pivot-i 0
           pivot (aget ratio 0)
           dir 0
           durs (transient [])]
      (if (>= i n)
        (persistent! durs)
        (let [v (aget ratio i)]
          (cond
            (and (pos? dir) (<= (/ v pivot) dn-mult))
            (recur (inc i) i v -1 (conj! durs (- i pivot-i)))

            (and (neg? dir) (>= (/ v pivot) up-mult))
            (recur (inc i) i v 1 (conj! durs (- i pivot-i)))

            (and (zero? dir) (>= (/ v pivot) up-mult))
            (recur (inc i) i v 1 durs)

            (and (zero? dir) (<= (/ v pivot) dn-mult))
            (recur (inc i) i v -1 durs)

            (and (pos? dir) (> v pivot))
            (recur (inc i) i v dir durs)

            (and (neg? dir) (< v pivot))
            (recur (inc i) i v dir durs)

            :else
            (recur (inc i) pivot-i pivot dir durs)))))))

(defn- median
  ^double [xs]
  (let [s (vec (sort xs))
        n (count s)
        mid (quot n 2)]
    (if (odd? n)
      (double (nth s mid))
      (/ (+ (double (nth s (dec mid)))
            (double (nth s mid)))
         2.0))))

(defn measure
  "Comparable ratio stats for one unordered pair, or nil.

  `a` and `b` are packed close maps. The overlap has to cover a full
  centering window."
  [a-ticker b-ticker correlation a b]
  (when-let [{:keys [^doubles ratio start end n]} (common-ratio a b)]
    (when-let [xs (centered ratio center-sessions)]
      (when-let [{:keys [low high span]} (band xs)]
        (let [years (years-between start end)
              durs (swing-sessions ratio deviation)
              months (when (seq durs)
                       (/ (median durs) (double sessions-per-month)))]
          (when (pos? years)
            {:tickers [a-ticker b-ticker]
             :correlation (round4 correlation)
             :years (round4 years)
             :low (round4 low)
             :high (round4 high)
             :span (round4 span)
             :deviations (round4 (/ (double (count durs)) years))
             :months (when months (round4 months))}))))))

(defn- write-edn!
  [path data]
  (let [file (io/file path)
        tmp (io/file (str path ".tmp"))]
    (some-> (.getParentFile file) .mkdirs)
    (spit tmp (with-out-str (pprint/pprint data)))
    (.delete file)
    (when-not (.renameTo tmp file)
      (throw (ex-info "could not write edn" {:path path})))
    path))

(defn- secs-since [t0]
  (/ (double (- (System/nanoTime) t0)) 1.0e9))

(defn run
  "Read nearby.edn, score every pair above 0.7, and write similar-behavior.edn.

  Pairs are ordered by the tightest centered band first."
  [_]
  (quiet-logs!)
  (let [t0 (System/nanoTime)
        nearby-file (io/file nearby-path)]
    (when-not (.exists nearby-file)
      (throw (ex-info "nearby.edn is missing. Run clj -X:nearness." {:path nearby-path})))
    (let [pairs (high-pairs (edn/read-string (slurp nearby-file)))
          tickers (into #{} (mapcat identity) (keys pairs))]
      (println (format "%d pairs above %.1f, %d tickers" (count pairs) min-correlation (count tickers)))
      (flush)
      (let [closes (load-closes tickers)
            rows (vec (keep (fn [[[a b] correlation]]
                              (let [sa (get closes a)
                                    sb (get closes b)]
                                (when (and sa sb)
                                  (measure a b correlation sa sb))))
                            pairs))
            ordered (vec (sort-by (fn [{:keys [span deviations tickers]}]
                                    [(double span)
                                     (- (double deviations))
                                     tickers])
                                  rows))
            data (array-map
                  :min-correlation min-correlation
                  :center-sessions center-sessions
                  :deviation deviation
                  :pairs ordered)]
        (write-edn! output-path data)
        (println (format "wrote %s (%d pairs) in %.1f s"
                         output-path (count ordered) (secs-since t0)))
        (flush)
        {:path output-path
         :pairs (count ordered)}))))
