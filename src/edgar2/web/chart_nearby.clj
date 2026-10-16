(ns edgar2.web.chart-nearby
  "Daily chart of a ticker and the peers in data/nearby.edn.

  The left column is the associated-tickers table: ticker, correlation,
  and a show checkbox. A peer starts checked when its correlation is
  above show-correlation.

  The right column is the shared pair chart: two linear panes over the
  selected number of years of daily closes. Five years is the default."
  (:require [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.web.chart :as chart]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.pair-chart :as pair-chart]
            [edgar2.web.ticker :as ticker]))

(def show-correlation
  "Peers with a higher correlation start with show checked."
  0.7)

(defn- show-default?
  [correlation]
  (and (number? correlation)
       (> (double correlation) (double show-correlation))))

(defn- years-param
  [req]
  (pair-chart/parse-years (get-in req [:hyper/route :path-params :years])))

(defn chart-nearby-payload
  [ticker years]
  (pair-chart/payload ticker
                      (mapv (fn [peer]
                              (assoc peer :show (show-default? (:correlation peer))))
                            (chart/nearby-for ticker))
                      years))

(defn- table-rows
  "Peer rows for the checkbox table, without the close series."
  [peers]
  (mapv #(select-keys % [:ticker :correlation :show :color :missing]) peers))

(h/defc nearby-table
  "Associated-tickers table. Checking a row tells the sibling pair-chart
  which tickers to draw, without a server round-trip.

  The selected ticker is always the black line on the chart."
  [{:keys [ticker peers years]}]
  (render
    [:div
     [:div.nearby-title "Associated tickers"]
     [:p.nearby-note]
     [:div.nearby-table]])
  (mount [root]
    (set! (.-ticker ctx) (or ticker ""))
    (set! (.-years ctx) (or years 5))
    (set! (.-peers ctx) (or peers (js/Array.)))
    (letfn [(finite? [v]
              (js/Number.isFinite v))
            (any-point? [arr]
              (and arr
                   (loop [i 0]
                     (cond
                       (>= i (.-length arr)) false
                       (finite? (aget arr i)) true
                       :else (recur (inc i))))))
            (init-show! []
              (let [show (js/Object.)
                    peers (.-peers ctx)
                    n (if peers (.-length peers) 0)]
                (dotimes [i n]
                  (let [p (aget peers i)]
                    (aset show (.-ticker p) (if (.-show p) true false))))
                (set! (.-show ctx) show)))
            (chart-el []
              (let [host (.-host (.getRootNode root))
                    parent (when host (.-parentElement host))]
                (when parent
                  (.querySelector parent "pair-chart"))))
            (push-show! []
              (when-let [chart (chart-el)]
                (let [peers (or (.-peers ctx) (js/Array.))
                      show (or (.-show ctx) (js/Object.))
                      names (js/Array.)]
                  (dotimes [i (.-length peers)]
                    (let [sym (.-ticker (aget peers i))]
                      (when (aget show sym)
                        (.push names sym))))
                  (.setAttribute chart "shown" (js/JSON.stringify names)))))
            (fill-table! []
              (let [box (.querySelector root ".nearby-table")
                    note (.querySelector root ".nearby-note")
                    peers (or (.-peers ctx) (js/Array.))
                    n (.-length peers)
                    sw (.createElement js/document "span")]
                (set! (.-textContent note) "")
                (set! (.-className sw) "nearby-swatch")
                (set! (.. sw -style -background) "#000")
                (.appendChild note sw)
                (.appendChild note (.createTextNode js/document
                                                    (str (.-ticker ctx) " is the black line")))
                (set! (.-textContent box) "")
                (if (zero? n)
                  (let [p (.createElement js/document "p")]
                    (set! (.-className p) "nearby-empty")
                    (set! (.-textContent p) "No associated tickers.")
                    (.appendChild box p))
                  (let [table (.createElement js/document "table")
                        thead (.createElement js/document "thead")
                        tbody (.createElement js/document "tbody")
                        head (.createElement js/document "tr")]
                    (doseq [[tag text class-name] [["th" "show" "show"]
                                                   ["th" "ticker" nil]
                                                   ["th" "correlation" "num"]]]
                      (let [cell (.createElement js/document tag)]
                        (set! (.-textContent cell) text)
                        (when class-name
                          (set! (.-className cell) class-name))
                        (.appendChild head cell)))
                    (.appendChild thead head)
                    (dotimes [i n]
                      (let [p (aget peers i)
                            sym (.-ticker p)
                            tr (.createElement js/document "tr")
                            td-show (.createElement js/document "td")
                            td-name (.createElement js/document "td")
                            td-corr (.createElement js/document "td")
                            input (.createElement js/document "input")
                            swatch (.createElement js/document "span")
                            link (.createElement js/document "a")]
                        (set! (.-className td-show) "show")
                        (set! (.-type input) "checkbox")
                        (set! (.-checked input) (if (aget (.-show ctx) sym) true false))
                        (.setAttribute input "aria-label" (str "Show " sym))
                        (.addEventListener input "change"
                                           (fn [ev]
                                             (aset (.-show ctx) sym (.. ev -target -checked))
                                             (push-show!)))
                        (.appendChild td-show input)
                        (set! (.-className swatch) "nearby-swatch")
                        (set! (.. swatch -style -background) (or (.-color p) "#999"))
                        (.appendChild td-name swatch)
                        (set! (.-className link) "nearby-ticker")
                        (set! (.-textContent link) sym)
                        (set! (.-href link)
                              (str "/company/chart-nearby/" (or (.-years ctx) 5)
                                   "?ticker=" (js/encodeURIComponent sym)))
                        (.addEventListener link "click"
                                           (fn [ev]
                                             (.preventDefault ev)
                                             (emit "open-ticker" {:ticker sym})))
                        (.appendChild td-name link)
                        (when (or (.-missing p)
                                  (and (.-closes p) (not (any-point? (.-closes p)))))
                          (let [miss (.createElement js/document "span")]
                            (set! (.-className miss) "nearby-missing")
                            (set! (.-textContent miss) "no prices")
                            (.appendChild td-name miss)))
                        (set! (.-className td-corr) "num")
                        (set! (.-textContent td-corr)
                              (if (finite? (.-correlation p))
                                (.toFixed (.-correlation p) 4)
                                ""))
                        (.appendChild tr td-show)
                        (.appendChild tr td-name)
                        (.appendChild tr td-corr)
                        (.appendChild tbody tr)))
                    (.appendChild table thead)
                    (.appendChild table tbody)
                    (.appendChild box table)))))]
      (init-show!)
      (fill-table!)
      (set! (.-loaded ctx) (or ticker ""))
      (set! (.-loadedYears ctx) (or years 5))
      (set! (.-initShow ctx) init-show!)
      (set! (.-fillTable ctx) fill-table!)
      (set! (.-pushShow ctx) push-show!)))
  (update [_root]
    (set! (.-ticker ctx) (or ticker ""))
    (set! (.-years ctx) (or years 5))
    (set! (.-peers ctx) (or peers (js/Array.)))
    (when (and (.-initShow ctx)
               (or (not= (.-loaded ctx) (or ticker ""))
                   (not= (.-loadedYears ctx) (or years 5))))
      ((.-initShow ctx))
      ((.-fillTable ctx))
      ((.-pushShow ctx))
      (set! (.-loaded ctx) (or ticker ""))
      (set! (.-loadedYears ctx) (or years 5)))))

(defn- open-nearby!
  "Open the nearby chart for `ticker` over `years`. Five years uses the
  bare route. Any other choice is in the path so the page remounts."
  [ticker years]
  (let [years (pair-chart/parse-years years)
        q (when ticker {:ticker ticker})]
    (if (= years 5)
      (effects/navigate! :company-chart-nearby {} q)
      (effects/navigate! :company-chart-nearby-years
                         {:years (str years)}
                         q))))

(defn- go-years!
  [years]
  (open-nearby! (ticker/current) years))

(defn- years-form
  [years]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
      (go-years! (:years $form-data)))}
   [:label "Years "
    [:select {:name "years"
              :data-on:change
              (h/action
                (go-years! (:years $form-data)))}
     (for [choice pair-chart/year-choices]
       [:option (cond-> {:key choice :value (str choice)}
                  (= choice years) (assoc :selected true))
        (str choice " year")])]]
   [:button {:type "submit"} "Show"]])

(defn chart-nearby-page
  [req]
  (let [years (years-param req)]
    (h/view
      {:mount (fn []
                (ticker/seed! req)
                (ticker/begin (fn [t] (chart-nearby-payload t years))))
       :render (fn [{:keys [result*]} req]
                 (let [{:keys [status ticker message data]} @result*]
                   [:div.page
                    (nav req)
                    (years-form years)
                    (case status
                      :loading [:p (str "Loading nearby chart for " ticker
                                        " (" years " year)…")]
                      :error [:p.error message]
                      :ready [:div.nearby-layout
                              (nearby-table {:class "nearby-side"
                                             :ticker (:ticker data)
                                             :years years
                                             :peers (table-rows (:peers data))
                                             :data-on:open-ticker
                                             (h/action
                                               (let [next (ticker/normalize (:ticker $detail))]
                                                 (when next
                                                   (reset! (ticker/ticker*) next)
                                                   (open-nearby! next years))))})
                              [:div.nearby-main
                               (pair-chart/pair-chart {:class "pair-chart"
                                                       :ticker (:ticker data)
                                                       :dates (:dates data)
                                                       :primary (:primary data)
                                                       :peers (:peers data)})]]
                      [:p "Enter a ticker."])]))
       :unmount (fn [{:keys [stop]}] (stop))})))
