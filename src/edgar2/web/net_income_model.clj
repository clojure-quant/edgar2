(ns edgar2.web.net-income-model
  "Company page: run edgar2.regression.db/ni-model for the tab ticker and
  list every key of the returned :model."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.db.ticker :as db-ticker]
            [edgar2.regression.db :as regression-db]
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
                    :ready (let [model (:model data)]
                             (if (seq model)
                               [:div
                                [:p.fin-header (str "CIK " (:cik data))]
                                (model-table model)]
                               [:p (str "No net-income model for " ticker ".")]))
                    [:p "Enter a ticker."])]))
     :unmount (fn [{:keys [stop]}] (stop))}))
