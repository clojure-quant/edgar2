(ns edgar2.web.pairs
  "Pairs from data/similar-behavior.edn, with the shared pair chart.

  The left column is every pair whose swing length is known. The right
  column is the same two-pane chart as chart-nearby, for the selected
  pair only. ticker-a is the black line. The ratio is ticker-b divided
  by ticker-a."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.pair-chart :as pair-chart]
            [edgar2.web.ticker :as ticker]))

(def pairs-path
  "Rows written by clj -X:similar-behavior."
  "data/similar-behavior.edn")

(defonce ^:private cache* (atom nil))

(defn- query-value
  [req k]
  (let [q (get-in req [:hyper/route :query-params])]
    (or (get q k) (get q (name k)))))

(defn rows
  "Pairs that have a swing length. Reread when the file changes.
  Empty when the file is missing."
  []
  (let [file (io/file pairs-path)]
    (if-not (.exists file)
      []
      (let [stamp (.lastModified file)
            cached @cache*]
        (when (or (nil? cached) (not= stamp (:stamp cached)))
          (reset! cache*
                  {:stamp stamp
                   :rows (vec (filter (comp some? :months)
                                      (:pairs (edn/read-string (slurp file)))))}))
        (:rows @cache*)))))

(defn- path-value
  [req k]
  (let [p (get-in req [:hyper/route :path-params])]
    (or (get p k) (get p (name k)))))

(defn- selection
  "The pair to chart, and the year window. A missing path falls back
  to the first row. Path params remount the page, which reloads the chart."
  [req rows]
  (let [years (pair-chart/parse-years (or (path-value req :years)
                                          (query-value req :years)))
        a (ticker/normalize (path-value req :a))
        b (ticker/normalize (path-value req :b))
        match (when (and a b)
                (some (fn [row]
                        (let [[ra rb] (:tickers row)]
                          (when (and (= a ra) (= b rb))
                            row)))
                      rows))
        row (or match (first rows))]
    {:years years
     :a (first (:tickers row))
     :b (second (:tickers row))
     :correlation (:correlation row)}))

(defn- open-pair!
  "Five years uses the path without a year. Any other window is in the
  path so the page remounts and reloads the chart."
  [a b years]
  (let [years (pair-chart/parse-years years)]
    (if (= years 5)
      (effects/navigate! :pairs-ab {:a a :b b} {})
      (effects/navigate! :pairs-ab-years
                         {:a a :b b :years (str years)}
                         {}))))

(defn- years-form
  [a b years]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
      (open-pair! a b (:years $form-data)))}
   [:label "Years "
    [:select {:name "years"
              :data-on:change
              (h/action
                (open-pair! a b (:years $form-data)))}
     (for [choice pair-chart/year-choices]
       [:option (cond-> {:key choice :value (str choice)}
                  (= choice years) (assoc :selected true))
        (str choice " year")])]]
   [:button {:type "submit"} "Show"]])

(defn- fmt
  [x]
  (if (number? x)
    (format "%.4f" (double x))
    ""))

(defn- pairs-table
  [rows a b years]
  [:div.pairs-side
   [:div.nearby-title (str (count rows) " pairs")]
   [:table
    [:thead
     [:tr
      [:th "ticker-a"]
      [:th "ticker-b"]
      [:th.num "correlation"]
      [:th.num "years"]
      [:th.num "low"]
      [:th.num "high"]
      [:th.num "span"]
      [:th.num "deviations"]
      [:th.num "months"]]]
    [:tbody
     (for [row rows]
       (let [[ra rb] (:tickers row)
             selected? (and (= ra a) (= rb b))]
         [:tr (cond-> {:key (str ra "/" rb)
                       :data-on:click (h/action
                                       (open-pair! ra rb years))}
                selected? (assoc :class "pairs-selected"))
          [:td ra]
          [:td rb]
          [:td.num (fmt (:correlation row))]
          [:td.num (fmt (:years row))]
          [:td.num (fmt (:low row))]
          [:td.num (fmt (:high row))]
          [:td.num (fmt (:span row))]
          [:td.num (fmt (:deviations row))]
          [:td.num (fmt (:months row))]]))]]])

(defn pairs-page
  [req]
  (let [table-rows (rows)
        sel (selection req table-rows)]
    (h/view
      {:mount (fn []
                (let [result* (atom {:status :empty})
                      alive? (atom true)
                      gen (atom 0)]
                  (h/watch! result*)
                  (when (and (:a sel) (:b sel))
                    (ticker/load! result* alive? gen
                                  (str (:a sel) " " (:b sel))
                                  #(pair-chart/payload (:a sel)
                                                       [{:ticker (:b sel)
                                                         :correlation (:correlation sel)
                                                         :show true}]
                                                       (:years sel))))
                  {:result* result*
                   :alive? alive?}))
       :render (fn [{:keys [result*]} req]
                 (let [shown (selection req table-rows)
                       {:keys [a b years]} shown
                       {:keys [status message data]} (when result* @result*)]
                   [:div.page
                    (nav req)
                    (years-form a b years)
                    [:div.pairs-layout
                     (if (seq table-rows)
                       (pairs-table table-rows a b years)
                       [:div.pairs-side
                        [:p.nearby-empty "No pairs in similar-behavior.edn."]])
                     [:div.pairs-main
                      (case status
                        :loading [:p (str "Loading " a " / " b
                                          " (" years " year)…")]
                        :error [:p.error message]
                        :ready (pair-chart/pair-chart {:class "pair-chart"
                                                       :ticker (:ticker data)
                                                       :dates (:dates data)
                                                       :primary (:primary data)
                                                       :peers (:peers data)})
                        [:p "Select a pair."])]]]))
       :unmount (fn [{:keys [alive?]}]
                  (when alive?
                    (reset! alive? false)))})))
