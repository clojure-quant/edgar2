(ns edgar2.web.eodhd-screen
  (:require [clojure.string :as str]
            [edgar2.eodhd.screen :as screen]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :name :label "Name"}
   {:key :days-ago :label "Days ago" :num? true}
   {:key :date :label "Date"}
   {:key :prior-high :label "11y high" :num? true}
   {:key :close :label "Close" :num? true}])

(defn- cell [v]
  (if (nil? v) "" (str v)))

(defn- table [{:keys [rows]}]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      (for [{:keys [key label num?]} columns]
        [:th {:key (str key)
              :class (when num? "num")}
         label])]]
    [:tbody
     (for [[i row] (map-indexed vector rows)]
       [:tr {:key i}
        (for [{:keys [key num?]} columns]
          [:td {:key (str key)
                :class (when num? "num")}
           (if (= key :ticker)
             (if-let [t (:ticker row)]
               [:a (ticker/company-link t) (str t)]
               "")
             (cell (get row key)))])])]]])

(defn eodhd-screen-page
  [_req]
  (let [loaded (try (screen/screen)
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))]
    (fn [req]
      [:div.page
       (nav req)
       [:h1 "Longterm up breakout"]
       (if-let [err (:error loaded)]
         [:p.error err]
         [:div
          [:pre.screen-notes (str/join "\n" (:lines loaded))]
          (when (seq (:rows loaded))
            (table loaded))])])))
