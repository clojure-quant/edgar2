(ns edgar2.web.chart
  "30-year price chart for the company ticker.

  Prices are the split-adjusted daily bars, rolled up to one candle per
  month so thirty years fit on the screen. Split markers come from the
  EODHD split files. Pixi draws both in a web component; the page passes
  the candles and the splits in as attributes.

  The annotation list can draw longterm fractal support and resistance:
  the optimum base from the full series, and every octave of that base
  that falls inside the price range on screen."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.eodhd.ingress :as ingress]
            [edgar2.algo.lt-breakout :as lt-breakout]
            [edgar2.algo.ltfsr :as ltfsr]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]
            [quanta.bar.db.nippy :as nippy]
            [tablecloth.api :as tc]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate ZoneId]))

(def ^:private ny (ZoneId/of "America/New_York"))

(def chart-years 30)

(defn- session-date
  [inst]
  (LocalDate/ofInstant inst ny))

(defn- px
  "Four decimal places, as a double JSON can encode."
  [x]
  (* 0.0001 (Math/round (* 10000.0 (double x)))))

(defn- ltfsr-annotation
  "Optimum fractal base for the whole split-adjusted series.

  The search uses ltfsr/search-base, ltfsr/search-step, and
  ltfsr/interval-count, the same bases the lt-fsr screen scores.
  The chart then draws the octaves of that base that sit inside the
  visible price range."
  [ds]
  (when (pos? (tc/row-count ds))
    (let [hit (ltfsr/position-search ltfsr/search-base ltfsr/search-step ltfsr/interval-count ds)]
      {:base (px (:base-price hit))
       :ratio (px (:ratio hit))})))

(defn- window-cutoff
  "Session date `chart-years` before the last bar in `ds`."
  [ds]
  (let [n (tc/row-count ds)
        last-day (session-date (nth (:date ds) (dec n)))]
    (.minusYears last-day chart-years)))

(defn- split-label
  "2.0 becomes \"2:1\". A reverse split below 1 becomes \"1:N\"."
  [factor]
  (let [f (double factor)
        whole (fn [n]
                (let [n (double n)
                      r (Math/rint n)]
                  (if (< (Math/abs (- n r)) 0.05)
                    (str (long r))
                    (format "%.2f" n))))]
    (cond
      (>= f 1.0) (str (whole f) ":1")
      (pos? f) (str "1:" (whole (/ 1.0 f)))
      :else "")))

(defn split-markers
  "Splits on or after `cutoff`. Each marker is the month key plus a ratio label."
  [asset cutoff]
  (let [path (str ingress/bardb-path
                  "_split_" (str/replace asset #"/" "_") "-us-d.nippy.gz")]
    (if-not (.exists (io/file path))
      []
      (->> (nippy/load-ds path)
           ds/mapseq-reader
           (keep (fn [row]
                   (let [d (session-date (:date row))
                         factor (double (:close row))]
                     (when (and (not (.isBefore d cutoff))
                                (pos? factor)
                                (not (== factor 1.0)))
                       {:t (format "%04d-%02d" (.getYear d) (.getMonthValue d))
                        :label (split-label factor)}))))
           (sort-by :t)
           vec))))

(defn monthly-bars
  "One OHLC candle per calendar month, from `cutoff` through the last bar.

  `ds` is split-adjusted daily bars, oldest first."
  [ds cutoff]
  (let [months (->> (ds/mapseq-reader ds)
                    (filter (fn [row]
                              (let [d (session-date (:date row))]
                                (and (not (.isBefore d cutoff))
                                     (pos? (double (:low row)))
                                     (pos? (double (:high row)))))))
                    (partition-by (fn [row]
                                    (let [d (session-date (:date row))]
                                      [(.getYear d) (.getMonthValue d)]))))]
    (mapv (fn [month]
            (let [d (session-date (:date (first month)))]
              {:t (format "%04d-%02d" (.getYear d) (.getMonthValue d))
               :o (px (:open (first month)))
               :c (px (:close (last month)))
               :h (px (apply max (map :high month)))
               :l (px (apply min (map :low month)))}))
          months)))

(h/defc price-chart
  "Pixi canvas. Attributes are the data; mount owns the Application.

  The annotation select is empty, ltfsr, or lt-breakout. ltfsr draws
  octave price levels. lt-breakout draws the prior high from the
  session that printed it through the newest bar."
  {:require [["https://cdn.jsdelivr.net/npm/pixi.js@8.14.3/dist/pixi.min.mjs" :as pixi]]}
  [{:keys [ticker bars splits ltfsr breakout]}]
  (event ::annotation [e]
    (set! (.-annotation ctx) (.. e -target -value))
    (when (.-redraw ctx)
      ((.-redraw ctx))))
  (render
    [:div.price-chart-layout
     [:div.price-chart-main
      [:div.price-chart-host]]
     [:aside.price-chart-side
      [:label.chart-annotation-label "Annotation"
       [:select.chart-annotation-select {:on {:change ::annotation}}
        [:option {:value ""} "empty"]
        [:option {:value "ltfsr"} "ltfsr"]
        [:option {:value "lt-breakout"} "lt-breakout"]]]
      [:div.chart-annotation-table]]])
  (mount [root]
    (let [host (.querySelector root ".price-chart-host")
          app (pixi/Application.)]
      (set! (.-app ctx) app)
      (set! (.-ticker ctx) (or ticker ""))
      (set! (.-bars ctx) bars)
      (set! (.-splits ctx) splits)
      (set! (.-ltfsr ctx) ltfsr)
      (set! (.-breakout ctx) breakout)
      (letfn [(label! [s size fill]
                (pixi/Text. {:text (str s)
                             :style {:fill fill
                                     :fontSize size
                                     :fontFamily "sans-serif"}}))
              (fmt [p]
                (cond
                  (>= p 100) (str (js/Math.round p))
                  (>= p 10) (.toFixed p 1)
                  :else (.toFixed p 2)))
              (cell [tag text class-name]
                (let [node (.createElement js/document tag)]
                  (set! (.-textContent node) text)
                  (when class-name
                    (set! (.-className node) class-name))
                  node))
              (fill-table! [box headers values]
                (let [table (.createElement js/document "table")
                      thead (.createElement js/document "thead")
                      tbody (.createElement js/document "tbody")
                      head (.createElement js/document "tr")
                      row (.createElement js/document "tr")]
                  (doseq [h headers]
                    (.appendChild head (cell "th" h "num")))
                  (doseq [v values]
                    (.appendChild row (cell "td" v "num")))
                  (.appendChild thead head)
                  (.appendChild tbody row)
                  (.appendChild table thead)
                  (.appendChild table tbody)
                  (.appendChild box table)))
              (annotation-table! [annotation]
                (let [box (.querySelector root ".chart-annotation-table")]
                  (when box
                    (set! (.-textContent box) "")
                    (cond
                      (and (= annotation "ltfsr") (.-ltfsr ctx))
                      (let [ann (.-ltfsr ctx)]
                        (fill-table! box
                                     ["base-price" "ratio"]
                                     [(.toFixed (.-base ann) 2)
                                      (.toFixed (.-ratio ann) 4)]))

                      (and (= annotation "lt-breakout") (.-breakout ctx))
                      (let [ann (.-breakout ctx)]
                        (fill-table! box
                                     ["11y high" "% above" "days"]
                                     [(.toFixed (.-high ann) 2)
                                      (.toFixed (.-pct ann) 2)
                                      (str (.-days ann))]))

                      (= annotation "lt-breakout")
                      (set! (.-textContent box) "No breakout.")))))
              (ticks [y-min y-max]
                (loop [pow (js/Math.floor (js/Math.log10 y-min))
                       acc []]
                  (let [base (js/Math.pow 10 pow)]
                    (if (> base y-max)
                      acc
                      (let [acc (if (and (>= base y-min) (<= base y-max))
                                  (conj acc base)
                                  acc)
                            v2 (* 2 base)
                            acc (if (and (>= v2 y-min) (<= v2 y-max))
                                  (conj acc v2)
                                  acc)
                            v5 (* 5 base)]
                        (recur (inc pow)
                               (if (and (>= v5 y-min) (<= v5 y-max))
                                 (conj acc v5)
                                 acc)))))))
              (octave-levels [base y-min y-max]
                ;; Walk /2 and *2 from the optimum base. Keep a level only
                ;; when its price sits inside the chart.
                (let [down (loop [p base
                                  acc []
                                  guard 0]
                             (if (or (< p y-min) (>= guard 64))
                               acc
                               (recur (/ p 2.0)
                                      (if (<= p y-max) (conj acc p) acc)
                                      (inc guard))))]
                  (loop [p (* 2.0 base)
                         acc down
                         guard 0]
                    (if (or (> p y-max) (>= guard 64))
                      acc
                      (recur (* 2.0 p)
                             (if (>= p y-min) (conj acc p) acc)
                             (inc guard))))))
              (draw-levels! [stage y-of pad-l plot-w y-min y-max]
                (let [ann (.-ltfsr ctx)
                      base (when ann (.-base ann))]
                  (when (and base (> base 0))
                    (let [bands (pixi/Graphics.)
                          marks (pixi/Graphics.)]
                      (doseq [p (octave-levels base y-min y-max)]
                        (let [y (y-of p)
                              y-top (y-of (* p 1.05))
                              y-bot (y-of (* p 0.95))
                              top (js/Math.min y-top y-bot)
                              bh (js/Math.max 1 (js/Math.abs (- y-bot y-top)))]
                          (.rect bands pad-l top plot-w bh)
                          (.fill bands {:color 0x5b8def :alpha 0.2})
                          (.moveTo marks pad-l y)
                          (.lineTo marks (+ pad-l plot-w) y)
                          (.stroke marks {:width 1.25 :color 0xd6e4ff})))
                      (.addChild stage bands)
                      (.addChild stage marks)))))
              (draw-breakout! [stage bars y-of x-of pad-l plot-w n]
                (let [ann (.-breakout ctx)
                      high (when ann (.-high ann))]
                  (when (and high (> high 0))
                    (let [month (.-t ann)
                          idx (loop [i 0]
                                (cond
                                  (>= i n) -1
                                  (= month (.-t (aget bars i))) i
                                  :else (recur (inc i))))
                          x0 (if (>= idx 0) (x-of idx) pad-l)
                          y (y-of high)
                          g (pixi/Graphics.)]
                      (.moveTo g x0 y)
                      (.lineTo g (+ pad-l plot-w) y)
                      (.stroke g {:width 1.75 :color 0x5ce1ff})
                      (.addChild stage g)))))
              (draw! []
                (let [annotation (or (.-annotation ctx) "")
                      stage (.-stage app)
                      screen (.-screen app)
                      w (.-width screen)
                      h (.-height screen)
                      _ (annotation-table! annotation)
                      bars (or (.-bars ctx) [])
                      n (or (.-length bars) 0)
                      pad-l 58
                      pad-r 16
                      pad-t 32
                      pad-b 28]
                  (.removeChildren stage)
                  (if (zero? n)
                    (let [g (pixi/Graphics.)
                          title (label! (str (.-ticker ctx) "  ·  30 years") 18 0xd7e0ea)]
                      (.rect g 24 48 (- w 48) (- h 72))
                      (.stroke g {:width 1 :color 0x3a4a5c})
                      (.addChild stage g)
                      (set! (.-x title) 28)
                      (set! (.-y title) 14)
                      (.addChild stage title))
                    (let [first (aget bars 0)
                          lastb (aget bars (dec n))
                          range (loop [i 0
                                       lo (.-l first)
                                       hi (.-h first)]
                                  (if (>= i n)
                                    [lo hi]
                                    (let [bar (aget bars i)]
                                      (recur (inc i)
                                             (js/Math.min lo (.-l bar))
                                             (js/Math.max hi (.-h bar))))))
                          lo (nth range 0)
                          hi (nth range 1)
                          log-lo (js/Math.log lo)
                          log-hi (let [v (js/Math.log hi)]
                                   (if (> v log-lo) v (+ log-lo 0.01)))
                          pad (* (- log-hi log-lo) 0.08)
                          y-min (js/Math.exp (- log-lo pad))
                          y-max (js/Math.exp (+ log-hi pad))
                          plot-h (- h pad-t pad-b)
                          plot-w (- w pad-l pad-r)
                          slot (/ plot-w n)
                          y-of (fn [p]
                                 (let [t (/ (- (js/Math.log p) (js/Math.log y-min))
                                            (- (js/Math.log y-max) (js/Math.log y-min)))]
                                   (+ pad-t (* (- 1 t) plot-h))))
                          x-of (fn [i] (+ pad-l (* (+ i 0.5) slot)))
                          grid (pixi/Graphics.)
                          candles (pixi/Graphics.)
                          title (label! (str (.-ticker ctx) "  ·  "
                                             (.-t first) " – " (.-t lastb)
                                             "  ·  monthly")
                                        14 0xd7e0ea)]
                      (set! (.-x title) 8)
                      (set! (.-y title) 6)
                      (.addChild stage title)
                      (doseq [p (ticks y-min y-max)]
                        (let [y (y-of p)
                              text (label! (fmt p) 11 0x8ea0b5)]
                          (.moveTo grid pad-l y)
                          (.lineTo grid (- w pad-r) y)
                          (.set (.-anchor text) 1 0.5)
                          (set! (.-x text) (- pad-l 6))
                          (set! (.-y text) y)
                          (.addChild stage text)))
                      (loop [i 0
                             labeled -100]
                        (when (< i n)
                          (let [bar (aget bars i)
                                year (js/parseInt (.slice (.-t bar) 0 4) 10)
                                month (.slice (.-t bar) 5)
                                want (or (zero? i)
                                         (= i (dec n))
                                         (and (= month "01") (= 0 (mod year 5))))
                                show (and want (> (- i labeled) 8))]
                            (when show
                              (let [x (x-of i)
                                    text (label! (str year) 11 0x8ea0b5)]
                                (.moveTo grid x pad-t)
                                (.lineTo grid x (+ pad-t plot-h))
                                (.set (.-anchor text) 0.5 0)
                                (set! (.-x text) x)
                                (set! (.-y text) (+ pad-t plot-h 4))
                                (.addChild stage text)))
                            (recur (inc i) (if show i labeled)))))
                      (.stroke grid {:width 1 :color 0x243044})
                      (.addChild stage grid)
                      (loop [i 0]
                        (when (< i n)
                          (let [bar (aget bars i)
                                open (.-o bar)
                                close (.-c bar)
                                up (>= close open)
                                color (if up 0x3dd68c 0xff5d5d)
                                x (x-of i)
                                body-w (js/Math.max 1 (- slot 1))
                                y-open (y-of open)
                                y-close (y-of close)
                                top (js/Math.min y-open y-close)
                                body-h (js/Math.max 1 (js/Math.abs (- y-close y-open)))]
                            (.moveTo candles x (y-of (.-h bar)))
                            (.lineTo candles x (y-of (.-l bar)))
                            (.stroke candles {:width 1 :color color})
                            (.rect candles (- x (/ body-w 2)) top body-w body-h)
                            (.fill candles {:color color})
                            (recur (inc i)))))
                      (.addChild stage candles)
                      (when (= annotation "ltfsr")
                        (draw-levels! stage y-of pad-l plot-w y-min y-max))
                      (when (= annotation "lt-breakout")
                        (draw-breakout! stage bars y-of x-of pad-l plot-w n))
                      (let [splits (or (.-splits ctx) [])
                            nsplit (or (.-length splits) 0)]
                        (loop [s 0]
                          (when (< s nsplit)
                            (let [sp (aget splits s)
                                  month (.-t sp)
                                  idx (loop [i 0]
                                        (cond
                                          (>= i n) -1
                                          (= month (.-t (aget bars i))) i
                                          :else (recur (inc i))))]
                              (when (>= idx 0)
                                (let [x (x-of idx)
                                      mark (pixi/Graphics.)
                                      text (label! (.-label sp) 12 0xf5c451)
                                      right (> x (- w 70))]
                                  (.moveTo mark x pad-t)
                                  (.lineTo mark x (+ pad-t plot-h))
                                  (.stroke mark {:width 1.5 :color 0xf5c451})
                                  (.poly mark [x (- pad-t 8)
                                               (+ x 5) pad-t
                                               x (+ pad-t 8)
                                               (- x 5) pad-t])
                                  (.fill mark {:color 0xf5c451})
                                  (.addChild stage mark)
                                  (if right
                                    (do (.set (.-anchor text) 1 0)
                                        (set! (.-x text) (- x 8)))
                                    (do (.set (.-anchor text) 0 0)
                                        (set! (.-x text) (+ x 8))))
                                  (set! (.-y text) (+ pad-t 2))
                                  (.addChild stage text)))
                              (recur (inc s))))))))))]
        (set! (.-redraw ctx) draw!))
      (-> (.init app {:background 0x0f1419
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
    (set! (.-bars ctx) bars)
    (set! (.-splits ctx) splits)
    (set! (.-ltfsr ctx) ltfsr)
    (set! (.-breakout ctx) breakout)
    (when (.-ready ctx)
      ((.-redraw ctx))))
  (unmount [_root]
    (when (.-app ctx)
      (.destroy (.-app ctx))
      (set! (.-app ctx) nil))))

(defn chart-payload
  [ticker]
  (let [ds (ingress/adjusted-bars ticker)]
    (when (or (nil? ds) (zero? (tc/row-count ds)))
      (throw (ex-info (str "No prices for " ticker) {:ticker ticker})))
    (let [cutoff (window-cutoff ds)]
      {:ticker ticker
       :bars (monthly-bars ds cutoff)
       :splits (split-markers ticker cutoff)
       :ltfsr (ltfsr-annotation ds)
       :breakout (lt-breakout/annotation ds)})))

(defn chart-page
  [req]
  (h/view
    {:mount (fn []
              (ticker/seed! req)
              (ticker/begin chart-payload))
     :render (fn [{:keys [result*]} req]
               (let [{:keys [status ticker message data]} @result*]
                 [:div.page
                  (nav req)
                  (case status
                    :loading [:p (str "Loading chart for " ticker "…")]
                    :error [:p.error message]
                    :ready [:div
                            (price-chart {:class "price-chart"
                                          :ticker ticker
                                          :bars (:bars data)
                                          :splits (:splits data)
                                          :ltfsr (:ltfsr data)
                                          :breakout (:breakout data)})]
                    [:p "Enter a ticker."])]))
     :unmount (fn [{:keys [stop]}] (stop))}))
