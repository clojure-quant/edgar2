(ns edgar2.eodhd.algo.lt-breakout
  "Longterm up breakout on one bar series.

  A split-adjusted high in the last 100 sessions above the prior 11-year
  high. The 11-year window ends where those 100 sessions start, so the
  series has to reach back at least 11 years. Days ago is how many
  sessions sit between that first cross and the newest bar.

  find-breakout takes bars and returns a hit or nil. Series loading
  lives in edgar2.eodhd.screen.

  print-quintiles prints the fractal position intervals of a series
  at a chosen base price. interval-count sets how many equal parts
  the band is split into."
  (:require [clojure.pprint :as pprint]
            [edgar2.eodhd.ltfsr :as ltfsr]
            [tech.v3.dataset :as ds]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.argops :as argops]
            [tech.v3.datatype.functional :as dfn])
  (:import [java.time Instant LocalDate ZoneId ZonedDateTime]))

(def high-years 11)
(def breakout-bars 100)

(def label
  "Summary word for one hit."
  "breakout")

(def notes
  "How a hit is chosen. Screen appends these under its load summary."
  [(format "The %d-year high excludes the last %d sessions." high-years breakout-bars)
   (format "A name needs history back to the start of those %d years." high-years)
   (format "A breakout is the first of those %d sessions whose high is above that level."
           breakout-bars)
   "Days ago counts sessions from that cross to the newest bar."])

(def ^:private ny (ZoneId/of "America/New_York"))

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

(defn find-breakout
  "First of the last 100 sessions whose high is above the prior 11-year high.

  `bars` is a tech.ml.dataset, or a sequence of maps, sorted oldest-first
  by :date and carrying :high. The 11-year window ends where those 100
  sessions start, and the series has to reach back to that date.

  Returns {:days-ago :date :high :close :prior-high}, or nil."
  [bars]
  (let [bars (ds/->dataset bars)
        n (ds/row-count bars)]
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

(defn- interval-table-row
  "One interval as prices inside the base band [base, 2*base]."
  [base n i count]
  (let [lo (* base (+ 1.0 (/ (double i) (double n))))
        hi (* base (+ 1.0 (/ (double (inc i)) (double n))))
        width (/ 10.0 (double n))]
    {:interval i
     :position (format "%.2f-%.2f" (* i width) (* (inc i) width))
     :low (format "%.2f" lo)
     :high (format "%.2f" hi)
     :count count}))

(def interval-columns
  [:interval :position :low :high :count])

(defn print-quintiles
  "Print `interval-count` equal position intervals of `bars` at `base-price`.

  Interval 0 is the bottom of [base-price, 2*base-price] and the last
  interval is the top. Closes in other octaves are folded into that
  band. The summary compares the average count of those two ends with
  the average count of the intervals between them."
  [base-price interval-count bars]
  (let [base (double base-price)
        counts (ltfsr/position-distribution base interval-count bars)
        n (count counts)
        {:keys [edges-avg others-avg ratio low-count high-count]}
        (ltfsr/interval-score counts)]
    (println (format "base %.2f  band %.2f .. %.2f  intervals %d  closes %d"
                     base base (* 2.0 base) n (reduce + counts)))
    (println (format "edges avg %.2f (low %d, high %d)  others avg %.2f  ratio %.4f"
                     edges-avg low-count high-count others-avg ratio))
    (pprint/print-table interval-columns
                        (mapv (fn [i c] (interval-table-row base n i c))
                              (range n)
                              counts))))
