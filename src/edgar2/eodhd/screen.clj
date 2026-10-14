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
            [tablecloth.api :as tc])
  (:import [java.time Instant LocalDate ZoneId ZonedDateTime]))

(def high-years 11)
(def breakout-bars 100)

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

(defn- session-rows [ds]
  (->> (tc/rows ds :as-maps)
       (filter #(and (as-instant (:date %)) (number? (:high %))))
       (sort-by #(as-instant (:date %)))
       vec))

(defn- find-breakout
  [bars]
  (let [bars (->> bars
                  (filter #(and (as-instant (:date %)) (number? (:high %))))
                  (sort-by #(as-instant (:date %)))
                  vec)
        n (count bars)]
    (when (> n breakout-bars)
      (let [recent (subvec bars (- n breakout-bars))
            before (subvec bars 0 (- n breakout-bars))
            start (as-instant (:date (first recent)))
            cutoff (minus-years start high-years)
            oldest (as-instant (:date (first before)))
            prior (filterv #(not (.isBefore (as-instant (:date %)) cutoff)) before)]
        (when (and (seq prior) (not (.isAfter oldest cutoff)))
          (let [prior-high (reduce max (map #(double (:high %)) prior))
                hit (first (keep-indexed
                            (fn [i row]
                              (when (> (double (:high row)) prior-high)
                                [i row]))
                            recent))]
            (when hit
              (let [[i row] hit]
                {:days-ago (- (dec breakout-bars) i)
                 :date (:date row)
                 :high (:high row)
                 :close (:close row)
                 :prior-high prior-high}))))))))

(defn- load-bars [asset]
  (try
    (ingress/adjusted-bars asset)
    (catch Exception _ nil)))

(defn- display [hit {:keys [code name]}]
  {:ticker code
   :name (or name "")
   :days-ago (:days-ago hit)
   :date (day-str (:date hit))
   :prior-high (price (:prior-high hit))
   :close (price (:close hit))})

(defn- compute []
  (let [equities (or (dataset/read-rows ingress/equities-path) [])]
    (if (empty? equities)
      {:rows []
       :n-equities 0
       :n-bars 0
       :lines ["No equity list yet. Run Bar download on Ingress EODHD."]}
      (let [acc (reduce (fn [acc equity]
                          (if-let [ds (load-bars (:code equity))]
                            (let [acc (update acc :n-bars inc)
                                  hit (find-breakout (session-rows ds))]
                              (if hit
                                (update acc :rows conj (display hit equity))
                                acc))
                            acc))
                        {:n-bars 0 :rows []}
                        equities)
            rows (->> (:rows acc)
                      (sort-by (juxt :days-ago :ticker))
                      vec)]
        {:rows rows
         :n-equities (count equities)
         :n-bars (:n-bars acc)
         :lines [(format "%d %s, %d with bars, %d %s"
                         (count equities)
                         (if (= 1 (count equities)) "equity" "equities")
                         (:n-bars acc)
                         (count rows)
                         (if (= 1 (count rows)) "breakout" "breakouts"))
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

  A series is a sequence of maps with :date and :high. Anything else
  (including the map `-X` passes) loads data/eodhd and prints the table.

  Usage: clj -X:eodhd-screen"
  ([] (longterm-up-breakout {}))
  ([bars]
   (if (sequential? bars)
     (find-breakout bars)
     (do
       (print-screen (screen))
       nil))))
