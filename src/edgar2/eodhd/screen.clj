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

(defonce ^:private cache* (atom nil))

(defn- day-str [dt]
  (cond
    (instance? Instant dt) (str (.toLocalDate (.atZone ^Instant dt ny)))
    (instance? LocalDate dt) (str dt)
    (instance? ZonedDateTime dt) (str (.toLocalDate (.withZoneSameInstant ^ZonedDateTime dt ny)))
    :else (str dt)))

(defn- present-val [v]
  (cond
    (or (instance? Instant v)
        (instance? LocalDate v)
        (instance? ZonedDateTime v))
    (day-str v)
    (and (number? v) (not (integer? v)))
    (format "%.2f" (double v))
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

(defn- hit-row [hit {:keys [code name]}]
  (assoc (update-vals hit present-val)
         :ticker code
         :name (or name "")))

(defn- row-key [row]
  (let [ago (:days-ago row)]
    [(if (number? ago) ago Long/MAX_VALUE)
     (or (:ticker row) "")]))

(defn- plural [word n]
  (if (= 1 n) word (str word "s")))

(defn- summary-lines [n-equities n-bars n-rows secs-load secs-calculate label notes]
  (into [(format "%d %s, %d with bars, %d %s"
                 n-equities
                 (if (= 1 n-equities) "equity" "equities")
                 n-bars
                 n-rows
                 (plural label n-rows))
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
      (hit-row hit equity))))

(defn- compute [algo label notes]
  (let [equities (or (dataset/read-rows ingress/equities-path) [])]
    (if (empty? equities)
      {:rows []
       :n-equities 0
       :n-bars 0
       :secs-load 0.0
       :secs-calculate 0.0
       :lines (into ["No equity list yet. Run Bar download on Ingress EODHD."] notes)}
      (let [t-load (System/nanoTime)
            loaded (parallel! (map load-task equities) load-parallel)
            secs-load (secs-since t-load)
            with-bars (filterv :ds loaded)
            t-calc (System/nanoTime)
            hits (parallel! (map #(calc-task algo %) with-bars) (cpu-parallel))
            secs-calculate (secs-since t-calc)
            rows (->> hits
                      (remove nil?)
                      (sort-by row-key)
                      vec)]
        {:rows rows
         :n-equities (count equities)
         :n-bars (count with-bars)
         :secs-load secs-load
         :secs-calculate secs-calculate
         :lines (summary-lines (count equities)
                               (count with-bars)
                               (count rows)
                               secs-load
                               secs-calculate
                               label
                               notes)}))))

(defn- stamp []
  [(.lastModified (io/file ingress/equities-path))
   (.lastModified (io/file ingress/bars-meta-path))])

(defn screen
  "Load adjusted bars for the equity list and call `algo` on each series.

  `algo` takes one bar dataset and returns a hit map or nil. Each hit is
  merged with :ticker and :name. A numeric :days-ago orders the rows.

  Options:
  :label  word for one hit in the summary (default \"hit\")
  :notes  strings appended after the load summary"
  ([algo] (screen algo {}))
  ([algo {:keys [label notes] :or {label "hit"}}]
   (let [notes (vec (or notes []))
         s (stamp)
         cached @cache*]
     (if (and cached
              (= s (:stamp cached))
              (identical? algo (:algo cached))
              (= label (:label cached))
              (= notes (:notes cached)))
       (:value cached)
       (let [value (compute algo label notes)]
         (reset! cache* {:stamp s :algo algo :label label :notes notes :value value})
         value)))))

(defn- columns [rows]
  (let [preferred [:ticker :name :days-ago :date :prior-high :close]
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
  or a symbol resolving to one.

  Usage: clj -X:eodhd-screen"
  [{:keys [algo label notes]}]
  (when-not algo
    (throw (ex-info "Pass :algo, a function or a namespaced symbol" {})))
  (print-screen (screen (resolve-fn algo)
                        {:label (or label "hit")
                         :notes (resolve-notes notes)}))
  nil)
