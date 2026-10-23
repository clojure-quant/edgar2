(ns edgar2.eodhd.screen
  "Load EODHD bar series and run an algo on each one.

  The algo is a function of a bar dataset (oldest-first) and returns a
  hit map or nil. This namespace loads the series and does not choose
  the calculation.

  Usage: clj -X:eodhd-screen"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.download.dataset :as dataset]
            [edgar2.eodhd.ingress :as ingress]
            [missionary.core :as m]
            [modular.log :as log]
            [quanta.missionary.parallel :refer [run-parallel]])
  (:import [java.time Instant LocalDate ZoneId ZonedDateTime]))

(log/timbre-config!
 {:min-level [[#{"quanta.bar.db.nippy"} :info]
              [#{"*"} :debug]]})

(def load-parallel
  "run-parallel holds a missionary semaphore so the blocking nippy reads
  stay on a fixed pool."
  200)

(def ^:private ny (ZoneId/of "America/New_York"))

(defonce ^:private bars-cache* (atom nil))
(defonce ^:private result-cache* (atom {}))

(defn- day-str [dt]
  (cond
    (instance? Instant dt) (str (.toLocalDate (.atZone ^Instant dt ny)))
    (instance? LocalDate dt) (str dt)
    (instance? ZonedDateTime dt) (str (.toLocalDate (.withZoneSameInstant ^ZonedDateTime dt ny)))
    :else (str dt)))

(defn- present-val
  [v decimals]
  (cond
    (or (instance? Instant v)
        (instance? LocalDate v)
        (instance? ZonedDateTime v))
    (day-str v)
    (and (number? v) (not (integer? v)))
    (format (str "%." (long (or decimals 2)) "f") (double v))
    :else v))

(defn- cpu-parallel []
  (max 1 (.availableProcessors (Runtime/getRuntime))))

(defn- parallel!
  "Run `tasks` with quanta.missionary.parallel/run-parallel and return a vector.
  An empty task list is not passed to join."
  [tasks n]
  (if (empty? tasks)
    []
    (vec (m/? (run-parallel tasks n)))))

(defn- secs-since [t0]
  (/ (double (- (System/nanoTime) t0)) 1.0e9))

(defn- tagged-hit [hit {:keys [code name]}]
  (assoc hit
         :ticker code
         :name (or name "")))

(defn- present-row [row decimals]
  (reduce-kv (fn [m k v]
               (assoc m k (present-val v (get decimals k))))
             {}
             row))

(defn- days-ago-key [row]
  (let [ago (:days-ago row)]
    [(if (number? ago) ago Long/MAX_VALUE)
     (or (:ticker row) "")]))

(defn- ratio-key
  "Sort key for :ratio. `descending?` puts the highest ratio first.
  A missing ratio stays last. Ties break on ticker, ascending."
  [descending? row]
  (let [r (:ratio row)
        ratio (when (and (number? r) (Double/isFinite (double r)))
                (double r))]
    [(cond
       (nil? ratio) Double/POSITIVE_INFINITY
       descending? (- ratio)
       :else ratio)
     (or (:ticker row) "")]))

(defn- sort-key-fn [sort-by descending?]
  (case sort-by
    nil days-ago-key
    :days-ago days-ago-key
    :ratio (fn [row] (ratio-key descending? row))
    (throw (ex-info "Unknown screen sort" {:sort-by sort-by}))))

(defn- plural [word n]
  (if (= 1 n) word (str word "s")))

(defn- summary-lines [n-equities n-bars n-scored n-shown secs-load secs-calculate label notes]
  (into [(format "%d %s, %d with bars, %d %s%s"
                 n-equities
                 (if (= 1 n-equities) "equity" "equities")
                 n-bars
                 n-scored
                 (plural label n-scored)
                 (if (< n-shown n-scored)
                   (format ", showing %d" n-shown)
                   ""))
         (format "load %.3f s, calculate %.3f s" secs-load secs-calculate)
         "Prices are split-adjusted."]
        notes))

(defn- load-task [equity]
  ;; via blk, not sp: adjusted-bars parks with m/?, and ? only blocks on a
  ;; plain thread. run-parallel's semaphore keeps this unbounded pool at
  ;; load-parallel.
  (m/via m/blk
    (try
      {:equity equity
       :ds (ingress/adjusted-bars (:code equity))}
      (catch Exception _
        {:equity equity :ds nil}))))

(defn- calc-task [algo {:keys [equity ds]}]
  (m/via m/cpu
    (when-let [hit (algo ds)]
      (tagged-hit hit equity))))

(defn- stamp []
  [(.lastModified (io/file ingress/equities-path))
   (.lastModified (io/file ingress/bars-meta-path))])

(defn- load-with-bars
  "Equity list plus adjusted bars, cached until the list or the bar
  catalog changes. A second algo on the same screen reuses the series."
  []
  (let [s (stamp)
        cached @bars-cache*]
    (if (and cached (= s (:stamp cached)))
      cached
      (let [equities (or (dataset/read-rows ingress/equities-path) [])
            t-load (System/nanoTime)
            loaded (parallel! (map load-task equities) load-parallel)
            value {:stamp s
                   :equities equities
                   :with-bars (filterv :ds loaded)
                   :secs-load (secs-since t-load)}]
        (reset! bars-cache* value)
        (reset! result-cache* {})
        value))))

(defn- screen-opts
  "Canonical options map. This map is the result-cache key, so two
  reports that differ in any option, including :reverse, do not share
  a cached table."
  [{:keys [label notes sort-by limit decimals reverse raw] :or {label "hit"}}]
  {:label label
   :notes (vec (or notes []))
   :sort-by sort-by
   :limit limit
   :decimals (or decimals {})
   :reverse (true? reverse)
   :raw (true? raw)})

(defn- compute [algo {:keys [label notes limit decimals raw] sort-key :sort-by reverse? :reverse} loaded]
  (let [{:keys [equities with-bars secs-load]} loaded
        t-calc (System/nanoTime)
        hits (parallel! (map #(calc-task algo %) with-bars) (cpu-parallel))
        secs-calculate (secs-since t-calc)
        scored (->> hits (remove nil?) (sort-by (sort-key-fn sort-key reverse?)))
        shown (if limit (take (long limit) scored) scored)
        rows (if raw
               (vec shown)
               (mapv #(present-row % decimals) shown))]
    {:rows rows
     :n-equities (count equities)
     :n-bars (count with-bars)
     :secs-load secs-load
     :secs-calculate secs-calculate
     :lines (summary-lines (count equities)
                           (count with-bars)
                           (count scored)
                           (count rows)
                           secs-load
                           secs-calculate
                           label
                           notes)}))

(defn screen
  "Load adjusted bars for the equity list and call `algo` on each series.

  `algo` takes one bar dataset and returns a hit map or nil. Each hit is
  merged with :ticker and :name before sorting. Numbers are formatted
  after the sort.

  Options:
  :label     word for one hit in the summary (default \"hit\")
  :notes     strings appended after the load summary
  :sort-by   :days-ago (default) or :ratio. Ties break on ticker.
  :reverse   true sorts :ratio descending (highest first). false keeps
             the lowest first.
  :limit     keep only this many rows after sorting
  :decimals  map of column key to decimal places (default 2)
  :raw       true leaves numbers as numbers. false formats them

  The bar load is shared across algos until the equity list or the bar
  catalog changes. Each algo's rows are cached on the stamp and the
  whole options map."
  ([algo] (screen algo {}))
  ([algo opts]
   (let [opts (screen-opts opts)
         loaded (load-with-bars)
         cache-key [(:stamp loaded) algo opts]]
     (if-let [cached (get @result-cache* cache-key)]
       cached
       (let [value (if (empty? (:equities loaded))
                     {:rows []
                      :lines (into ["No equity list yet. Run Bar download on Ingress EODHD."]
                                   (:notes opts))}
                     (compute algo opts loaded))]
         (swap! result-cache* assoc cache-key value)
         value)))))

(defn score
  "Run `algo` on the series for `items` and return {:rows :lines}.

  `items` are {:ticker :name :equity} from the eodhd universe. Bars come
  from the shared load cache. This does not use the result cache. The
  screen runner stores the rows, and its cache key is the screen id,
  the resolved parameters (including :reverse), and the input stamp.
  Recalculate always runs `algo` again. `:raw` true leaves numbers
  unformatted."
  [items algo opts]
  (let [opts (screen-opts opts)
        loaded (load-with-bars)]
    (if (empty? (:equities loaded))
      {:rows []
       :lines (into ["No equity list yet. Run Bar download on Ingress EODHD."]
                    (:notes opts))}
      (let [codes (set (keep :ticker items))]
        (compute algo
                 opts
                 (assoc loaded
                        :equities (filterv (comp codes :code) (:equities loaded))
                        :with-bars (filterv (comp codes :code :equity) (:with-bars loaded))))))))

(defn- columns [rows]
  (let [preferred [:ticker :name :days-ago :date :prior-high :prior-date :high-ago-days :close :ratio :base-price :octaves :years-history]
        present (set (mapcat keys rows))]
    (vec (distinct (concat (filter present preferred)
                           (filter present (mapcat keys rows)))))))

(defn- print-screen [result]
  (doseq [line (:lines result)]
    (println line))
  (when (seq (:rows result))
    (println)
    (pprint/print-table (columns (:rows result)) (:rows result))))

(defn- resolve-fn [x]
  (cond
    (fn? x) x
    (symbol? x) (if-let [v (requiring-resolve x)]
                  (deref v)
                  (throw (ex-info "Cannot resolve screen algo" {:algo x})))
    :else (throw (ex-info "Screen :algo must be a function or a symbol" {:algo x}))))

(defn- resolve-notes [x]
  (cond
    (nil? x) []
    (string? x) [x]
    (sequential? x) (vec x)
    (symbol? x) (if-let [v (requiring-resolve x)]
                  (let [value (deref v)]
                    (if (sequential? value)
                      (vec value)
                      (throw (ex-info "Screen notes var must be a sequence of strings"
                                      {:notes x}))))
                  (throw (ex-info "Cannot resolve screen notes" {:notes x})))
    :else (throw (ex-info "Screen :notes must be strings or a symbol" {:notes x}))))

(defn run
  "Print a screen. `:algo` is a function or a namespaced symbol that
  resolves to one. The function takes a bar dataset and returns a hit
  map or nil.

  `:label` names a hit in the summary. `:notes` is a vector of strings
  or a symbol resolving to one. `:sort-by` is :days-ago or :ratio.
  `:reverse` true sorts :ratio descending. `:limit` keeps that many
  rows. `:decimals` is a map of column key to decimal places.

  Usage: clj -X:eodhd-screen"
  [{:keys [algo label notes sort-by limit decimals reverse]}]
  (when-not algo
    (throw (ex-info "Pass :algo, a function or a namespaced symbol" {})))
  (print-screen (screen (resolve-fn algo)
                        {:label (or label "hit")
                         :notes (resolve-notes notes)
                         :sort-by sort-by
                         :limit limit
                         :decimals decimals
                         :reverse reverse}))
  nil)
