(ns edgar2.web.net-income-model
  "Company page: run edgar2.regression.db/ni-model for the tab ticker and
  list every key of the returned :model."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [tech.v3.dataset :as ds]
            [edgar2.db.ticker :as db-ticker]
            [edgar2.financials.facts-ui :as facts-ui]
            [edgar2.regression.db :as regression-db]
            [edgar2.web.chart-vegalite :as chart-vegalite]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]))

(defn display-value
  "Readable string for a model value. Keeps hiccup safe: never returns a
  vector/number child."
  [v]
  (cond
    (nil? v) ""
    (keyword? v) (name v)
    (boolean? v) (str v)
    (number? v) (cond
                  (Double/isNaN (double v)) "NaN"
                  (Double/isInfinite (double v)) (if (pos? (double v)) "Inf" "-Inf")
                  :else (str v))
    (sequential? v) (str/join ", " (map str v))
    :else (str v)))

(defn model-rows
  "Sorted [key-label value-label] pairs for a model map."
  [model]
  (->> model
       (sort-by (fn [[k _]] (name k)))
       (mapv (fn [[k v]] [(name k) (display-value v)]))))

(defn- model-table
  [model]
  [:div.table-wrap
   [:table
    [:thead [:tr [:th "key"] [:th "value"]]]
    [:tbody
     (for [[k v] (model-rows model)]
       [:tr {:key k} [:td k] [:td.num v]])]]])

(defn year-rows
  "Oldest-first `{:end :revenue :net-income}` rows from a model dataset."
  [ds]
  (if ds
    (->> (ds/rows ds {:nil-missing? true})
         reverse)
    []))

(defn- dataset-table
  [ds]
  (let [rows (year-rows (facts-ui/format-columns ds))
        fields [["Revenue" :revenue]
                ["Net Income" :net-income]
                ["EstimateNI" :estimate ]]]
    [:div.table-wrap
     [:table
      [:thead
       [:tr
        [:th "field"]
        (for [r rows] [:th {:key (:end r)} (:end r)])]]
      [:tbody
       (for [[label k] fields]
         [:tr {:key (name k)}
          [:td label]
          (for [r rows]
            [:td.num {:key (:end r)} (display-value (get r k))])])]]]))

(defn- normalize-future-ends
  "Return `rows` with numeric future markers in `:end` (\"2\", \"3\", ...)
  expanded to ISO dates `base+N`, where `base` is the latest ISO year in the
  rows. ISO dates pass through unchanged, so Vega's temporal axis places
  future estimates after the historical periods."
  [rows]
  (let [iso? #(re-matches #"\d{4}-\d{2}-\d{2}" (str %))
        marker? #(re-matches #"\d{1,2}" (str %))
        years (->> rows (map (comp str :end)) (filter iso?)
                   (map #(parse-long (subs % 0 4))))
        base (when (seq years) (apply max years))]
    (mapv (fn [r]
            (let [e (str (:end r))
                  n (when (and base (marker? e)) (parse-long e))]
              (assoc r :end (if n (str (+ base n) "-12-31") e))))
          rows)))

(defn chart-values
  "Long-form `{:end :series :value}` rows (raw dollars, oldest-first) for a
  model dataset. A nil figure is skipped; an all-nil period is already absent
  from `year-rows`."
  [ds]
  (let [fields [["Revenue" :revenue] 
                ["Net Income" :net-income]
                ["EstimateNI" :estimate]]]
    (vec (for [r (normalize-future-ends (year-rows ds))
               [label k] fields
               :let [v (get r k)]
               :when (number? v)]
           {:end (:end r) :series label :value v}))))

(defn chart-spec
  "Vega-Lite dual-Y-axis chart: Revenue (brown line) on the left axis and Net
  Income plus EstimateNI (black line) on the right axis, over the fiscal period
  ends in raw dollars. Net Income is drawn as a zero-anchored bar, green when
  positive and red when negative; EstimateNI keeps a dot per marker."
  [ds]
  {:width 1200
   :height 300
   :data {:values (chart-values ds)}
   :resolve {:scale {:y "independent"}}
   :layer
   [{:transform [{:filter "datum.series === 'Revenue'"}]
     :mark {:type "line" :point {:size 40}}
     :encoding
     {:x {:field "end" :type "nominal"
          ; :type "temporal"  ;:axis {:format "%Y" :title "Year"}
          }
      :y {:field "value" :type "quantitative"
          :axis {:title "Revenue (USD)" :format "$,.3s" :orient "left"}}
      :color {:field "series" :type "nominal"
              :scale {:domain ["Revenue" "Net Income" "EstimateNI"]
                      :range ["#8B4513" "#1f77b4" "#000000"]}
              :legend {:title nil}}
      :tooltip [{:field "end" :type "nominal"
                 ;:type "temporal"  :format "%Y-%m-%d"
                 }
                {:field "series" :type "nominal"}
                {:field "value" :type "quantitative" :format "$,.0f"}]}}
    {:layer
     [{:transform [{:filter "datum.series === 'Net Income'"}]
       :mark {:type "bar" :size 10 :opacity 0.75}
       :encoding
       {:x {:field "end" :type "nominal"
            ; :type "temporal"  ;:axis {:format "%Y" :title "Year"}
            }
        :y {:field "value" :type "quantitative"
            :axis {:title "Net Income / EstimateNI (USD)" :format "$,.3s" :orient "right"}}
        :y2 {:datum 0}
        :fill {:condition {:test "datum.value > 0" :value "#2ca02c"}
               :value "#d62728"}
        :tooltip [{:field "end" :type "nominal"
                   ;:type "temporal"  :format "%Y-%m-%d"
                   }
                  {:field "series" :type "nominal"}
                  {:field "value" :type "quantitative" :format "$,.0f"}]}}
      {:transform [{:filter "datum.series === 'EstimateNI'"}]
       :mark {:type "line" :point {:size 40}}
       :encoding
       {:x {:field "end" :type "nominal"
            ; :type "temporal"  ;:axis {:format "%Y" :title "Year"}
            }
        :y {:field "value" :type "quantitative"
            :axis {:title "Net Income / EstimateNI (USD)" :format "$,.3s" :orient "right"}}
        :color {:field "series" :type "nominal"
                :scale {:domain ["Revenue" "Net Income" "EstimateNI"]
                        :range ["#8B4513" "#1f77b4" "#000000"]}
                :legend {:title nil}}
        :tooltip [{:field "end" :type "nominal"
                   ;:type "temporal"  :format "%Y-%m-%d"
                   }
                  {:field "series" :type "nominal"}
                  {:field "value" :type "quantitative" :format "$,.0f"}]}}]
     :resolve {:scale {:y "shared"}}}]})

(defn net-income-model-page
  [req]
  (h/view
    {:mount (fn []
              (ticker/seed! req)
              (ticker/begin
                (fn [t] (regression-db/ni-model (db-ticker/ticker->cik t)))))
     :render (fn [{:keys [result*]} req]
               (let [{:keys [status ticker data message]} @result*]
                 [:div.page
                  (nav req)
                  [:h1 "Net income model"]
                  (case status
                    :loading [:p (str "Loading net income model for " ticker "…")]
                    :error [:p.error message]
                    :ready (let [model (:model data)
                                 ds (:combined-ds data)]
                             (if (and ds (pos? (ds/row-count ds)))
                               [:div
                                [:p.fin-header (str "CIK " (:cik data))]
                                (when (seq model) (model-table model))
                                (dataset-table ds)
                                (chart-vegalite/chart-vegalite
                                  {:spec (chart-spec ds)})]
                               [:p (str "No net-income model for " ticker "."
                                        "Keys: " (keys data)
                                        )]))
                    [:p "Enter a ticker."])]))
     :unmount (fn [{:keys [stop]}] (stop))}))
