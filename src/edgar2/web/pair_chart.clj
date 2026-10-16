(ns edgar2.web.pair-chart
  "Two-pane daily chart for a ticker and one or more peers.

  The top pane is each close indexed to 100 on that series' first day.
  The selected ticker is black. The lower pane is each peer close divided
  by the selected close, indexed to 1 on that ratio's first day.

  chart-nearby passes every associated ticker and hides some of them with
  the shown attribute. The pairs page passes the one selected pair.
  Both pages share this chart."
  (:require [hyper.core :as h]
            [edgar2.eodhd.ingress :as ingress]
            [edgar2.study.ratio :as ratio]
            [tablecloth.api :as tc]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate ZoneId]
           [java.util.concurrent Callable Executors]))

(def year-choices
  "Windows the chart can show. Five years is the default."
  [5 10 15 20 25 30])

(def line-colors
  "One color per peer, in order."
  ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd"
   "#8c564b" "#e377c2" "#17becf" "#bcbd22" "#7f7f7f"
   "#393b79" "#637939" "#8c6d31" "#843c39" "#7b4173"
   "#3182bd" "#e6550d" "#31a354" "#de2d26" "#756bb1"])

(def ^:private ny (ZoneId/of "America/New_York"))

(defn parse-years
  "One of year-choices, or 5 when `s` is not one of them."
  [s]
  (let [n (try
            (Long/parseLong (str s))
            (catch Exception _
              nil))]
    (if (some #(= n %) year-choices) n 5)))

(defn- session-date
  ^LocalDate [inst]
  (LocalDate/ofInstant inst ny))

(defn- positive?
  [x]
  (and (number? x)
       (let [d (double x)]
         (and (Double/isFinite d) (pos? d)))))

(defn- window-start
  "Session date `years` before the last bar in `ds`."
  ^LocalDate [ds years]
  (let [n (tc/row-count ds)
        last-day (session-date (nth (:date ds) (dec n)))]
    (.minusYears last-day (long years))))

(defn- closes-by-day
  "Map of YYYY-MM-DD to positive close, for sessions on or after `cutoff`."
  [ds ^LocalDate cutoff]
  (when (and ds (pos? (tc/row-count ds)))
    (into {}
          (keep (fn [row]
                  (let [d (session-date (:date row))
                        c (:close row)]
                    (when (and (not (.isBefore ^LocalDate d cutoff))
                               (positive? c))
                      [(str d) (double c)]))))
          (ds/mapseq-reader ds))))

(defn- align
  "Closes for `dates`, or nil where `by-day` has no session."
  [dates by-day]
  (let [by-day (or by-day {})]
    (mapv (fn [day] (get by-day day)) dates)))

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
  [ticker cutoff]
  (try
    (some-> (ingress/adjusted-bars ticker) (closes-by-day cutoff))
    (catch Exception _
      nil)))

(defn payload
  "Indexed closes for `ticker` and `peers` over `years`.

  Each peer is a map with :ticker and, when known, :correlation and :show.
  A peer without :show is shown. The selected close is :primary, indexed
  to 100. Each peer's :ratio is its close divided by the selected close,
  indexed to 1. :missing is true when that peer has no positive close in
  the window."
  [ticker peers years]
  (let [years (parse-years years)
        ds (ingress/adjusted-bars ticker)]
    (when (or (nil? ds) (zero? (tc/row-count ds)))
      (throw (ex-info (str "No prices for " ticker) {:ticker ticker})))
    (let [cutoff (window-start ds years)
          primary-map (closes-by-day ds cutoff)]
      (when (empty? primary-map)
        (throw (ex-info (str "No prices for " ticker) {:ticker ticker})))
      (let [dates (vec (sort (keys primary-map)))
            raw (mapv primary-map dates)
            peers (vec peers)
            by-ticker (into {}
                            (parallel-map
                             (fn [{:keys [ticker]}]
                               [ticker (load-closes ticker cutoff)])
                             peers))]
        {:ticker ticker
         :dates dates
         :primary (ratio/index-to raw 100.0)
         :peers (mapv (fn [peer color]
                        (let [aligned (align dates (get by-ticker (:ticker peer)))
                              closes (ratio/index-to aligned 100.0)]
                          {:ticker (:ticker peer)
                           :correlation (double (or (:correlation peer) 0))
                           :show (if (contains? peer :show)
                                   (boolean (:show peer))
                                   true)
                           :color color
                           :missing (not (some number? closes))
                           :closes closes
                           :ratio (ratio/index-to (ratio/close-ratio raw aligned) 1.0)}))
                      peers
                      (cycle line-colors))}))))

(h/defc pair-chart
  "Two linear panes. Peers in `shown` are drawn. When `shown` is omitted,
  a peer is drawn unless its show flag is false.

  The selected ticker is the black line on the top pane."
  {:require [["https://cdn.jsdelivr.net/npm/pixi.js@8.14.3/dist/pixi.min.mjs" :as pixi]]}
  [{:keys [ticker dates primary peers shown]}]
  (render
    [:div.pair-chart-host])
  (mount [root]
    (let [host (.querySelector root ".pair-chart-host")
          app (pixi/Application.)]
      (set! (.-app ctx) app)
      (set! (.-ticker ctx) (or ticker ""))
      (set! (.-dates ctx) (or dates (js/Array.)))
      (set! (.-primary ctx) (or primary (js/Array.)))
      (set! (.-peers ctx) (or peers (js/Array.)))
      (set! (.-shown ctx) shown)
      (letfn [(finite? [v]
                (js/Number.isFinite v))
              (label! [s size fill]
                (pixi/Text. {:text (str s)
                             :style {:fill fill
                                     :fontSize size
                                     :fontFamily "sans-serif"}}))
              (fmt [p]
                (let [a (js/Math.abs p)
                      r (js/Math.round p)]
                  (cond
                    (< (js/Math.abs (- p r)) 0.05) (str r)
                    (>= a 10) (.toFixed p 1)
                    (>= a 1) (.toFixed p 2)
                    :else (.toFixed p 3))))
              (hex [s]
                (js/parseInt (.slice (str s) 1) 16))
              (extent [arrs]
                (loop [ai 0
                       lo js/Infinity
                       hi (- js/Infinity)
                       seen false]
                  (if (>= ai (.-length arrs))
                    (when seen [lo hi])
                    (let [arr (aget arrs ai)
                          m (if arr (.-length arr) 0)
                          step (loop [i 0
                                      lo lo
                                      hi hi
                                      seen seen]
                                 (if (>= i m)
                                   [lo hi seen]
                                   (let [v (aget arr i)]
                                     (if (finite? v)
                                       (recur (inc i)
                                              (js/Math.min lo v)
                                              (js/Math.max hi v)
                                              true)
                                       (recur (inc i) lo hi seen)))))]
                      (recur (inc ai) (nth step 0) (nth step 1) (nth step 2))))))
              (padded [[lo hi]]
                (let [span (- hi lo)
                      span (if (< span 1e-6)
                             (js/Math.max (js/Math.abs hi) 1)
                             span)
                      pad (* span 0.08)]
                  [(- lo pad) (+ hi pad)]))
              (linear-ticks [y-min y-max]
                (let [span (- y-max y-min)]
                  (if (or (not (finite? span)) (<= span 0))
                    []
                    (let [rough (/ span 4)
                          pow (js/Math.pow 10 (js/Math.floor (js/Math.log10 rough)))
                          err (/ rough pow)
                          step (* pow (cond
                                        (< err 1.5) 1
                                        (< err 3) 2
                                        (< err 7) 5
                                        :else 10))
                          start (* step (js/Math.ceil (/ y-min step)))]
                      (loop [v start
                             acc []
                             guard 0]
                        (if (or (> v (+ y-max (* step 0.01))) (>= guard 24))
                          acc
                          (recur (+ v step)
                                 (if (and (>= v (- y-min (* step 0.001)))
                                          (<= v (+ y-max (* step 0.001))))
                                   (conj acc v)
                                   acc)
                                 (inc guard))))))))
              (year-indexes [dates n]
                (let [min-gap (js/Math.max 40 (js/Math.round (/ n 14)))]
                  (loop [i 0
                         last-i -1000
                         acc []]
                    (if (>= i n)
                      acc
                      (let [year (.slice (aget dates i) 0 4)
                            prev (when (pos? i) (.slice (aget dates (dec i)) 0 4))
                            boundary (or (zero? i) (not= year prev))
                            ok (and boundary (> (- i last-i) min-gap))]
                        (recur (inc i)
                               (if ok i last-i)
                               (if ok (conj acc i) acc)))))))
              (stroke-series! [stage values y-of x-of color width]
                (when values
                  (let [g (pixi/Graphics.)
                        n (.-length values)
                        any (loop [i 0
                                   pen false
                                   any false]
                              (if (>= i n)
                                any
                                (let [v (aget values i)]
                                  (if-not (finite? v)
                                    (recur (inc i) false any)
                                    (do
                                      (if pen
                                        (.lineTo g (x-of i) (y-of v))
                                        (.moveTo g (x-of i) (y-of v)))
                                      (recur (inc i) true true))))))]
                    (when any
                      (.stroke g {:width width
                                  :color color
                                  :join "round"
                                  :cap "round"})
                      (.addChild stage g)))))
              (y-scale [y-min y-max top height]
                (let [den (- y-max y-min)
                      den (if (> den 0) den 1)]
                  (fn [v]
                    (let [t (/ (- v y-min) den)]
                      (+ top (* (- 1 t) height))))))
              (y-labels! [stage ticks y-of top height pad-l]
                (doseq [p ticks]
                  (let [y (y-of p)]
                    (when (and (>= y (- top 1)) (<= y (+ top height 1)))
                      (let [text (label! (fmt p) 11 0x3e5166)]
                        (.set (.-anchor text) 1 0.5)
                        (set! (.-x text) (- pad-l 6))
                        (set! (.-y text) y)
                        (.addChild stage text))))))
              (visible [peers shown-list]
                (let [allow (when shown-list
                              (let [o (js/Object.)]
                                (dotimes [i (.-length shown-list)]
                                  (aset o (aget shown-list i) true))
                                o))
                      out (js/Array.)]
                  (dotimes [i (if peers (.-length peers) 0)]
                    (let [p (aget peers i)]
                      (when (if allow
                              (aget allow (.-ticker p))
                              (not= (.-show p) false))
                        (.push out p))))
                  out))
              (draw! []
                (let [stage (.-stage app)
                      screen (.-screen app)
                      w (.-width screen)
                      h (.-height screen)
                      dates (or (.-dates ctx) (js/Array.))
                      primary (or (.-primary ctx) (js/Array.))
                      peers (or (.-peers ctx) (js/Array.))
                      n (.-length dates)
                      pad-l 58
                      pad-r 14
                      pad-t 28
                      gap 26
                      pad-b 24]
                  (.removeChildren stage)
                  (if (zero? n)
                    (let [text (label! (str (.-ticker ctx) "  ·  no prices") 16 0x1a1a1a)]
                      (set! (.-x text) 16)
                      (set! (.-y text) 16)
                      (.addChild stage text))
                    (let [avail (- h pad-t gap pad-b)
                          top-h (js/Math.max 40 (js/Math.round (* avail 0.60)))
                          bot-h (js/Math.max 40 (- avail top-h))
                          top0 pad-t
                          bot0 (+ pad-t top-h gap)
                          plot-w (- w pad-l pad-r)
                          slot (/ plot-w n)
                          x-of (fn [i] (+ pad-l (* (+ i 0.5) slot)))
                          first-d (aget dates 0)
                          last-d (aget dates (dec n))
                          title (label! (str (.-ticker ctx) "  ·  "
                                             first-d " – " last-d
                                             "  ·  daily  ·  indexed to 100")
                                        13 0x1a1a1a)
                          ratio-title (label! (str "associated / " (.-ticker ctx)
                                                   "  ·  indexed to 1")
                                              13 0x1a1a1a)
                          shown (visible peers (.-shown ctx))
                          top-arrs (js/Array.)
                          _ (.push top-arrs primary)
                          _ (dotimes [i (.-length shown)]
                              (.push top-arrs (.-closes (aget shown i))))
                          bot-arrs (js/Array.)
                          _ (dotimes [i (.-length shown)]
                              (.push bot-arrs (.-ratio (aget shown i))))
                          top-ext (extent top-arrs)
                          bot-ext (extent bot-arrs)
                          top-range (when top-ext (padded top-ext))
                          bot-range (when bot-ext (padded bot-ext))
                          years (year-indexes dates n)
                          grid (pixi/Graphics.)
                          frame (pixi/Graphics.)]
                      (set! (.-x title) 8)
                      (set! (.-y title) 4)
                      (.addChild stage title)
                      (set! (.-x ratio-title) 8)
                      (set! (.-y ratio-title) (+ top0 top-h 4))
                      (.addChild stage ratio-title)
                      (.rect frame pad-l top0 plot-w top-h)
                      (.rect frame pad-l bot0 plot-w bot-h)
                      (.stroke frame {:width 1 :color 0xd0d7e2})
                      (.addChild stage frame)
                      (when top-range
                        (let [[y-min y-max] top-range
                              y-of (y-scale y-min y-max top0 top-h)]
                          (doseq [p (linear-ticks y-min y-max)]
                            (let [y (y-of p)]
                              (when (and (>= y top0) (<= y (+ top0 top-h)))
                                (.moveTo grid pad-l y)
                                (.lineTo grid (- w pad-r) y))))
                          (y-labels! stage (linear-ticks y-min y-max) y-of top0 top-h pad-l)))
                      (when bot-range
                        (let [[y-min y-max] bot-range
                              y-of (y-scale y-min y-max bot0 bot-h)]
                          (doseq [p (linear-ticks y-min y-max)]
                            (let [y (y-of p)]
                              (when (and (>= y bot0) (<= y (+ bot0 bot-h)))
                                (.moveTo grid pad-l y)
                                (.lineTo grid (- w pad-r) y))))
                          (y-labels! stage (linear-ticks y-min y-max) y-of bot0 bot-h pad-l)))
                      (doseq [i years]
                        (let [x (x-of i)
                              text (label! (.slice (aget dates i) 0 4) 11 0x3e5166)]
                          (.moveTo grid x top0)
                          (.lineTo grid x (+ top0 top-h))
                          (.moveTo grid x bot0)
                          (.lineTo grid x (+ bot0 bot-h))
                          (.set (.-anchor text) 0.5 0)
                          (set! (.-x text) x)
                          (set! (.-y text) (+ bot0 bot-h 4))
                          (.addChild stage text)))
                      (.stroke grid {:width 1 :color 0xe6edf3})
                      (.addChild stage grid)
                      (when top-range
                        (let [[y-min y-max] top-range
                              y-of (y-scale y-min y-max top0 top-h)]
                          (dotimes [i (.-length shown)]
                            (let [p (aget shown i)]
                              (stroke-series! stage (.-closes p) y-of x-of (hex (.-color p)) 1.75)))
                          (stroke-series! stage primary y-of x-of 0x000000 2.25)))
                      (if bot-range
                        (let [[y-min y-max] bot-range
                              y-of (y-scale y-min y-max bot0 bot-h)]
                          (dotimes [i (.-length shown)]
                            (let [p (aget shown i)]
                              (stroke-series! stage (.-ratio p) y-of x-of (hex (.-color p)) 1.75))))
                        (let [text (label! "No associated tickers selected." 13 0x5c6b7a)]
                          (set! (.-x text) (+ pad-l 8))
                          (set! (.-y text) (+ bot0 8))
                          (.addChild stage text)))))))]
        (set! (.-redraw ctx) draw!))
      (-> (.init app {:background 0xffffff
                      :antialias true
                      :resizeTo host
                      :autoDensity true
                      :resolution (or (.-devicePixelRatio js/window) 1)})
          (.then (fn []
                   (.appendChild host (.-canvas app))
                   (set! (.-ready ctx) true)
                   ((.-redraw ctx))
                   (.on (.-renderer app) "resize" (.-redraw ctx)))))))
  (update [_root]
    (set! (.-ticker ctx) (or ticker ""))
    (set! (.-dates ctx) (or dates (js/Array.)))
    (set! (.-primary ctx) (or primary (js/Array.)))
    (set! (.-peers ctx) (or peers (js/Array.)))
    (set! (.-shown ctx) shown)
    (when (.-ready ctx)
      ((.-redraw ctx))))
  (unmount [_root]
    (when (.-app ctx)
      (.destroy (.-app ctx))
      (set! (.-app ctx) nil))))
