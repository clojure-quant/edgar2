(ns edgar2.eodhd.screen
  "Longterm up breakout: a split-adjusted high in the last 100 sessions
  above the prior 11-year high.

  The 11-year high does not include those 100 sessions. The series has to
  reach back at least 11 years. Days ago is how many sessions sit between
  that first cross and the newest bar.

  Usage: clj -X:eodhd-screen"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.download.dataset :as dataset]
            [edgar2.eodhd.ingress :as ingress]
            [missionary.core :as m]
            [modular.log :as log]
            [quanta.missionary.parallel :refer [run-parallel]]
            [tech.v3.dataset :as ds]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.argops :as argops]
            [tech.v3.datatype.functional :as dfn])
  (:import [java.time Instant LocalDate ZoneId ZonedDateTime]))

(def high-years 11)
(def breakout-bars 100)

(log/timbre-config!
 {:min-level [[#{"quanta.bar.db.nippy"} :info]
              [#{"*"} :debug]]})

(def load-parallel
  "run-parallel holds a missionary semaphore so the blocking nippy reads
  stay on a fixed pool."
  200)

(def ^:private ny (ZoneId/of "America/New_York"))

(defonce ^:private cache* (atom {:stamp nil :value nil}))

(defn- day-str [dt]
  (cond
    (instance? Instant dt) (str (.toLocalDate (.atZone ^Instant dt ny)))
    (instance? LocalDate dt) (str dt)
    (instance? ZonedDateTime dt) (str (.toLocalDate (.withZoneSameInstant ^ZonedDateTime dt ny)))
    :else (str dt)))

(defn- price [x]
  (when (number? x)
    (format "%.2f" (double x))))

(defn- as-instant [dt]
  (cond
    (instance? Instant dt) dt
    (instance? ZonedDateTime dt) (.toInstant ^ZonedDateTime dt)
    (instance? LocalDate dt) (.toInstant (.atStartOfDay ^LocalDate dt ny))
    :else nil))

(defn- minus-years [dt years]
  (.toInstant (.minusYears (.atZone ^Instant (as-instant dt) ny) (long years))))

(defn- col-at [dataset colname idx]
  ((ds/column dataset colname) idx))

(defn- first-index [pred col]
  (let [idxs (argops/argfilter pred col)]
    (when (pos? (dtype/ecount idxs))
      (long (nth idxs 0)))))

(defn- find-breakout
  "First of the last 100 sessions whose high is above the prior 11-year high.

  `bars` is a tech.ml.dataset sorted oldest-first by :date. The 11-year
  window ends where those 100 sessions start, and the series has to reach
  back to that date."
  [bars]
  (let [n (ds/row-count bars)]
    (when (> n breakout-bars)
      (let [recent (ds/tail bars breakout-bars)
            before (ds/head bars (- n breakout-bars))
            start (as-instant (col-at recent :date 0))
            cutoff (minus-years start high-years)
            oldest (as-instant (col-at before :date 0))
            prior (ds/filter-column before :date
                                    #(not (.isBefore (as-instant %) cutoff)))]
        (when (and (pos? (ds/row-count prior))
                   (not (.isAfter oldest cutoff)))
          (let [prior-high (double (dfn/reduce-max (ds/column prior :high)))
                i (first-index #(> (double %) prior-high)
                               (ds/column recent :high))]
            (when i
              {:days-ago (- (dec breakout-bars) i)
               :date (col-at recent :date i)
               :high (col-at recent :high i)
               :close (when-let [close-col (:close recent)]
                        (close-col i))
               :prior-high prior-high})))))))

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

(defn- display [hit {:keys [code name]}]
  {:ticker code
   :name (or name "")
   :days-ago (:days-ago hit)
   :date (day-str (:date hit))
   :prior-high (price (:prior-high hit))
   :close (price (:close hit))})

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

(defn- calc-task [{:keys [equity ds]}]
  (m/via m/cpu
    (when-let [hit (find-breakout ds)]
      (display hit equity))))

(defn- compute []
  (let [equities (or (dataset/read-rows ingress/equities-path) [])]
    (if (empty? equities)
      {:rows []
       :n-equities 0
       :n-bars 0
       :secs-load 0.0
       :secs-calculate 0.0
       :lines ["No equity list yet. Run Bar download on Ingress EODHD."]}
      (let [t-load (System/nanoTime)
            loaded (parallel! (map load-task equities) load-parallel)
            secs-load (secs-since t-load)
            with-bars (filterv :ds loaded)
            t-calc (System/nanoTime)
            hits (parallel! (map calc-task with-bars) (cpu-parallel))
            secs-calculate (secs-since t-calc)
            rows (->> hits
                      (remove nil?)
                      (sort-by (juxt :days-ago :ticker))
                      vec)]
        {:rows rows
         :n-equities (count equities)
         :n-bars (count with-bars)
         :secs-load secs-load
         :secs-calculate secs-calculate
         :lines [(format "%d %s, %d with bars, %d %s"
                         (count equities)
                         (if (= 1 (count equities)) "equity" "equities")
                         (count with-bars)
                         (count rows)
                         (if (= 1 (count rows)) "breakout" "breakouts"))
                 (format "load %.3f s, calculate %.3f s" secs-load secs-calculate)
                 "Prices are split-adjusted."
                 "The 11-year high excludes the last 100 sessions."
                 "A name needs history back to the start of those 11 years."
                 "A breakout is the first of those 100 sessions whose high is above that level."
                 "Days ago counts sessions from that cross to the newest bar."]}))))

(defn- stamp []
  [(.lastModified (io/file ingress/equities-path))
   (.lastModified (io/file ingress/bars-meta-path))])

(defn screen
  "Table of longterm-up-breakout names from the files on disk."
  []
  (let [s (stamp)]
    (if (= s (:stamp @cache*))
      (:value @cache*)
      (let [value (compute)]
        (reset! cache* {:stamp s :value value})
        value))))

(defn- print-screen [result]
  (doseq [line (:lines result)]
    (println line))
  (when (seq (:rows result))
    (println)
    (pprint/print-table [:ticker :name :days-ago :date :prior-high :close]
                        (:rows result))))

(defn longterm-up-breakout
  "The breakout for one bar series, or the universe table when `bars` is not a series.

  A series is a dataset, or a sequence of maps, with :date and :high,
  sorted oldest-first.
  Anything else (including the map `-X` passes) loads data/eodhd and prints the table.

  Usage: clj -X:eodhd-screen"
  ([] (longterm-up-breakout {}))
  ([bars]
   (if (sequential? bars)
     (find-breakout (ds/->dataset bars))
     (do
       (print-screen (screen))
       nil))))
