(ns edgar2.screen.nearness
  "Similar stocks from aligned daily log returns.

  load-returns reads the last 15 years of split-adjusted closes, aligns
  them to the US daily calendar with :fill-forward-start-backward, and
  turns closes into log returns. correlations is the n by n Pearson
  matrix. A pair is 0.0 when either name has less than 5 years of
  returns. nearby keeps the 20 highest correlations per ticker.
  clusters cuts an average-linkage tree on correlation distance.

  The ticker list for `clj -X:nearness` is the EODHD equity list:
  latest-session turnover of at least 5 million, and a bar file on disk.

  Usage: clj -X:nearness"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.eodhd.ingress :as ingress]
            [edgar2.screen.universe :as universe]
            [missionary.core :as m]
            [modular.log :as log]
            [quanta.calendar.ds.window :as calendar-ds]
            [quanta.calendar.window :as calendar]
            [quanta.missionary.parallel :refer [run-parallel]]
            [tablecloth.api :as tc]
            [tick.core :as t])
  (:import [java.time Instant LocalDate ZoneId ZonedDateTime]
           [java.time.temporal ChronoUnit]
           [smile.clustering HierarchicalClustering]
           [smile.clustering.linkage UPGMALinkage]))

(set! *warn-on-reflection* true)

(log/timbre-config!
 {:min-level [[#{"quanta.bar.db.nippy"} :warn]
              [#{"*"} :info]]})

(def history-years
  "How many years of closes load-returns aligns."
  15)

(def min-years
  "Both names need this many years of real bars before a correlation is computed."
  5)

(def nearby-n
  "How many peers to keep per ticker, highest correlation first."
  20)

(def cluster-cut
  "Average-linkage cut on correlation distance sqrt((1 - corr) / 2).
  0.5 is a correlation of 0.5."
  0.5)

(def nearby-path "data/nearby.edn")
(def clusters-path "data/clusters.edn")

(def ^:private load-parallel 64)
(def ^:private ny (ZoneId/of "America/New_York"))
(def ^:private logged-short* (atom #{}))

(defn- secs-since [t0]
  (/ (double (- (System/nanoTime) t0)) 1.0e9))

(defn- local-date
  ^LocalDate [dt]
  (.toLocalDate (.atZone ^Instant dt ny)))

(defn- years-between
  "Whole years from `start` to `end` in New York. Negative when `end` is first."
  [start end]
  (long (.until (local-date start) (local-date end) ChronoUnit/YEARS)))

(defn- fmt-dt [dt]
  (str (local-date dt)))

(defn five-years?
  "True when the real bars in `series` span at least `min-years`."
  [{:keys [start-dt end-dt]}]
  (and (instance? Instant start-dt)
       (instance? Instant end-dt)
       (>= (years-between start-dt end-dt) (long min-years))))

(defn- log-short!
  "Log once per ticker that its pairs cannot be calculated."
  [{:keys [ticker start-dt end-dt]}]
  (let [[before] (swap-vals! logged-short* conj ticker)]
    (when-not (contains? before ticker)
      (let [years (if (and start-dt end-dt) (years-between start-dt end-dt) 0)]
        (println (format "%s cannot be calculated: %d %s of returns from %s to %s (need %d); correlation 0.0"
                         ticker years
                         (if (= 1 years) "year" "years")
                         (if start-dt (fmt-dt start-dt) "-")
                         (if end-dt (fmt-dt end-dt) "-")
                         (long min-years)))
        (flush)))))

(defn- window-range
  "Last `history-years`, as instants, ending now in New York."
  []
  (let [^ZoneId zone ny
        end (ZonedDateTime/now zone)
        start (.minusYears end (long history-years))]
    {:start (.toInstant start)
     :end (.toInstant end)}))

(defn- us-calendar
  "US daily close instants from `start` through `end`, oldest first."
  [{:keys [start end]}]
  (-> (calendar/date-range->window [:us :d] {:start start :end end})
      calendar-ds/window->ds))

(defn- in-window? [dt start end]
  (and (instance? Instant dt)
       (not (t/< dt start))
       (not (t/> dt end))))

(defn- clip-bars
  "Date and close inside [`start` `end`], oldest first. Nil when empty."
  [ds start end]
  (let [clipped (tc/select-rows
                 ds
                 (fn [row]
                   (in-window? (:date row) start end)))]
    (when (pos? (tc/row-count clipped))
      (-> clipped
          (tc/select-columns [:date :close])
          (tc/order-by [:date])))))

(defn- load-one
  "Split-adjusted bars for `ticker` inside the window, or nil."
  [ticker start end]
  (try
    (when-let [ds (ingress/adjusted-bars ticker)]
      (when (pos? (tc/row-count ds))
        (clip-bars ds start end)))
    (catch Exception ex
      (println (format "skip %s: %s" ticker (ex-message ex)))
      (flush)
      nil)))

(defn- load-bars
  "Map of ticker to clipped bar dataset. Tickers that fail to load are absent."
  [tickers start end]
  (let [tasks (mapv (fn [ticker]
                      (m/via m/blk
                        (when-let [ds (load-one ticker start end)]
                          [ticker ds])))
                    tickers)
        loaded (if (empty? tasks)
                 []
                 (vec (m/? (run-parallel tasks load-parallel))))]
    (into {} (remove nil?) loaded)))

(defn- date-at
  ^Instant [ds i]
  (let [dt (nth (:date ds) i)]
    (when-not (instance? Instant dt)
      (throw (ex-info "bar date is not an instant" {:date dt :index i})))
    dt))

(defn- index-on-or-after
  "Index of the first calendar date that is not before `dt`."
  [dates dt]
  (let [n (count dates)]
    (loop [lo 0 hi n]
      (if (>= lo hi)
        (when (< lo n) lo)
        (let [mid (quot (+ lo hi) 2)]
          (if (t/< (nth dates mid) dt)
            (recur (inc mid) hi)
            (recur lo mid)))))))

(defn- closes
  ^doubles [ds]
  (let [col (:close ds)
        n (count col)
        out (double-array n)]
    (dotimes [i n]
      (let [v (nth col i)]
        (aset out i
              (if (and (number? v) (Double/isFinite (double v)) (pos? (double v)))
                (double v)
                Double/NaN))))
    out))

(defn- log-returns
  "Natural log returns. Index 0 is NaN. A repeated close (a filled session) is 0."
  ^doubles [^doubles px]
  (let [n (alength px)
        out (double-array n Double/NaN)]
    (loop [i 1]
      (when (< i n)
        (let [a (aget px (dec i))
              b (aget px i)]
          (aset out i
                (if (and (Double/isFinite a) (Double/isFinite b) (pos? a) (pos? b))
                  (Math/log (/ b a))
                  Double/NaN)))
        (recur (inc i))))
    out))

(defn load-returns
  "Aligned log returns for `tickers` over the last 15 years.

  Each map is {:ticker :returns :start-dt :end-dt}. :returns is a double
  array with one value per US session in the window. :start-dt and
  :end-dt are the first and last sessions that have a real bar, before
  backward fill. Names with no bars in the window are left out."
  [tickers]
  (let [t0 (System/nanoTime)
        {:keys [start end] :as range} (window-range)
        calendar (us-calendar range)
        dates (:date calendar)
        n-cal (count dates)
        _ (println (format "calendar %s to %s, %d sessions"
                           (fmt-dt (nth dates 0))
                           (fmt-dt (nth dates (dec n-cal)))
                           n-cal))
        _ (flush)
        bars (load-bars tickers start end)
        _ (println (format "loaded %d/%d series in %.1f s"
                           (count bars) (count tickers) (secs-since t0)))
        _ (println "aligning")
        _ (flush)
        aligned (calendar-ds/aligned-ds-map calendar bars :fill-forward-start-backward)]
    (mapv (fn [ticker]
            (let [raw (get bars ticker)
                  ds (get aligned ticker)
                  n (tc/row-count raw)
                  start-dt (date-at raw 0)
                  end-dt (date-at raw (dec n))
                  start-idx (index-on-or-after dates start-dt)
                  end-idx (index-on-or-after dates end-dt)]
              (when (or (nil? ds)
                        (nil? start-idx)
                        (nil? end-idx)
                        (not= (nth dates start-idx) start-dt)
                        (not= (nth dates end-idx) end-dt)
                        (not= (tc/row-count ds) n-cal))
                (throw (ex-info "aligned series does not match the calendar"
                                {:ticker ticker
                                 :start-dt start-dt
                                 :end-dt end-dt
                                 :start-idx start-idx
                                 :end-idx end-idx
                                 :rows (when ds (tc/row-count ds))
                                 :calendar n-cal})))
              {:ticker ticker
               :returns (log-returns (closes ds))
               :start-dt start-dt
               :end-dt end-dt
               :start-idx start-idx
               :end-idx end-idx}))
          (filterv bars tickers))))

(defn- pearson
  "Pearson correlation of `x` and `y` on the half-open index range [from, to).
  Non-finite points are skipped. 0.0 when fewer than two points remain or
  either series has no variance."
  ^double [^doubles x ^doubles y from to]
  (loop [i (long from)
         s 0.0
         sx 0.0
         sy 0.0
         sxx 0.0
         syy 0.0
         sxy 0.0]
    (if (>= i (long to))
      (if (< s 2.0)
        0.0
        (let [varx (- sxx (/ (* sx sx) s))
              vary (- syy (/ (* sy sy) s))
              cov (- sxy (/ (* sx sy) s))]
          (if (or (<= varx 0.0) (<= vary 0.0))
            0.0
            (let [c (/ cov (Math/sqrt (* varx vary)))]
              (if (Double/isFinite c) c 0.0)))))
      (let [xi (aget x i)
            yi (aget y i)]
        (if (and (Double/isFinite xi) (Double/isFinite yi))
          (recur (inc i) (inc s) (+ sx xi) (+ sy yi)
                 (+ sxx (* xi xi)) (+ syy (* yi yi)) (+ sxy (* xi yi)))
          (recur (inc i) s sx sy sxx syy sxy))))))

(defn correlation
  "Pearson correlation of two `load-returns` maps on the sessions where both
  have a real bar.

  Returns 0.0 when either name has less than 5 years of returns, and logs
  that name once."
  [a b]
  (cond
    (= (:ticker a) (:ticker b)) 1.0
    (and (five-years? a) (five-years? b))
    (let [from (inc (max (long (:start-idx a)) (long (:start-idx b))))
          to (inc (min (long (:end-idx a)) (long (:end-idx b))))]
      (if (< from to)
        (pearson (:returns a) (:returns b) from to)
        0.0))
    :else
    (do
      (when-not (five-years? a) (log-short! a))
      (when-not (five-years? b) (log-short! b))
      0.0)))

(defn correlations
  "n by n Pearson matrix for `series`, same order as `series`.

  The diagonal is 1.0. A pair that cannot be calculated is 0.0."
  [series]
  (let [series (vec series)
        n (count series)
        t0 (System/nanoTime)
        ^"[[D" rows (make-array Double/TYPE n n)]
    (reset! logged-short* #{})
    (dotimes [i n]
      (aset rows i i 1.0)
      (dotimes [j i]
        (let [c (double (correlation (nth series i) (nth series j)))]
          (aset rows i j c)
          (aset rows j i c)))
      (when (or (= (inc i) n) (zero? (mod (inc i) 100)))
        (println (format "correlated %d/%d in %.1f s" (inc i) n (secs-since t0)))
        (flush)))
    rows))

(defn- round4
  ^double [x]
  (/ (double (Math/round (* 10000.0 (double x)))) 10000.0))

(defn nearby
  "Map of ticker to at most 20 {:ticker :correlation} maps, highest first.

  A name with less than 5 years maps to []. Peers are other names whose
  correlation was calculated."
  [series matrix]
  (let [series (vec series)
        n (count series)
        ^"[[D" matrix matrix
        eligible (boolean-array (map five-years? series))
        tickers (mapv :ticker series)]
    (into (sorted-map)
          (map (fn [i]
                 (let [ticker (nth tickers i)]
                   [ticker
                    (if-not (aget eligible i)
                      []
                      (->> (range n)
                           (keep (fn [j]
                                   (when (and (not= i (long j)) (aget eligible j))
                                     {:ticker (nth tickers j)
                                      :correlation (aget matrix i (long j))})))
                           (sort-by (fn [row]
                                      [(- (double (:correlation row))) (:ticker row)]))
                           (take nearby-n)
                           (mapv #(update % :correlation round4))))]))
               (range n)))))

(defn- corr-distance
  ^double [c]
  (let [x (max -1.0 (min 1.0 (double c)))]
    (Math/sqrt (* 0.5 (- 1.0 x)))))

(defn- proximity
  "Correlation distance between the eligible rows of `matrix`."
  ^"[[D" [matrix eligible-idx]
  (let [n (count eligible-idx)
        ^"[[D" matrix matrix
        ^"[[D" prox (make-array Double/TYPE n n)]
    (dotimes [a n]
      (aset prox a a 0.0)
      (dotimes [b a]
        (let [i (long (nth eligible-idx a))
              j (long (nth eligible-idx b))
              d (corr-distance (aget matrix i j))]
          (aset prox a b d)
          (aset prox b a d))))
    prox))

(defn- labels->clusters
  [labels tickers]
  (->> (map vector labels tickers)
       (group-by first)
       (map (fn [[_ members]]
              {:tickers (vec (sort (map second members)))}))
       (sort-by (fn [{:keys [tickers]}]
                  [(- (count tickers)) (first tickers)]))
       vec))

(defn clusters
  "Average-linkage clusters of names with at least 5 years of returns.

  Distance is sqrt((1 - corr) / 2). The tree is cut at `cluster-cut`.
  Each cluster is {:tickers [...]}."
  [series matrix]
  (let [series (vec series)
        eligible-idx (vec (keep-indexed (fn [i s] (when (five-years? s) i)) series))
        tickers (mapv (fn [i] (:ticker (nth series i))) eligible-idx)
        n (count tickers)]
    (cond
      (zero? n) []
      (= n 1) [{:tickers tickers}]
      :else
      (let [prox (proximity matrix eligible-idx)
            hc (HierarchicalClustering/fit (UPGMALinkage. prox))
            labels (try
                     (.partition hc (double cluster-cut))
                     (catch IllegalArgumentException _
                       ;; The whole tree sits below the cut: one cluster.
                       (int-array n)))]
        (labels->clusters labels tickers)))))

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

(defn- turnover-tickers
  "EODHD equities with a bar file. The list is already turnover >= 5 million."
  []
  (let [{:keys [items lines]} (universe/eodhd {})]
    (doseq [line lines]
      (println line))
    (flush)
    (mapv :ticker items)))

(defn run
  "Correlate the EODHD turnover universe and write nearby.edn and clusters.edn."
  [_]
  (let [t0 (System/nanoTime)
        tickers (turnover-tickers)]
    (when (empty? tickers)
      (throw (ex-info "No EODHD equities with bar files. Run clj -X:ingress-eodhd." {})))
    (println (format "nearness for %d equities with turnover >= 5 million" (count tickers)))
    (flush)
    (let [series (load-returns tickers)
          _ (when (empty? series)
              (throw (ex-info "No bar series loaded." {})))
          matrix (correlations series)
          peers (nearby series matrix)
          groups (clusters series matrix)]
      (write-edn! nearby-path peers)
      (write-edn! clusters-path {:linkage :average
                                 :distance :correlation
                                 :cut cluster-cut
                                 :clusters groups})
      (println (format "wrote %s (%d tickers) and %s (%d clusters) in %.1f s"
                       nearby-path (count peers)
                       clusters-path (count groups)
                       (secs-since t0)))
      (flush)
      {:nearby nearby-path
       :clusters clusters-path
       :n (count series)
       :clusters-n (count groups)})))
