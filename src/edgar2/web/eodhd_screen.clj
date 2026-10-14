(ns edgar2.web.eodhd-screen
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.eodhd.algo.lt-breakout :as lt]
            [edgar2.eodhd.algo.ltfsr :as ltfsr]
            [edgar2.eodhd.screen :as screen]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]))

(def screens
  [{:id :breakout
    :title "Longterm up breakout"
    :columns [{:key :ticker :label "Ticker"}
              {:key :name :label "Name"}
              {:key :days-ago :label "Days ago" :num? true}
              {:key :date :label "Date"}
              {:key :prior-high :label "11y high" :num? true}
              {:key :prior-date :label "High date"}
              {:key :high-ago-days :label "High ago" :num? true}
              {:key :close :label "Close" :num? true}]}
   {:id :ltfsr
    :title "lt-fsr"
    :columns [{:key :ticker :label "Ticker"}
              {:key :name :label "Name"}
              {:key :close :label "Close" :num? true}
              {:key :ratio :label "Ratio" :num? true}
              {:key :base-price :label "Base price" :num? true}
              {:key :octaves :label "Octaves" :num? true}
              {:key :years-history :label "Years" :num? true}]}])

(defn- cell [v]
  (if (nil? v) "" (str v)))

(defn- table [{:keys [columns rows]}]
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

(defn- load-screen [id]
  (case id
    :breakout (screen/screen lt/find-breakout
                             {:label lt/label :notes lt/notes})
    :ltfsr (screen/screen ltfsr/find-ltfsr
                          {:label ltfsr/label
                           :notes ltfsr/notes
                           :sort-by :ratio
                           :limit ltfsr/lowest-n
                           :decimals {:ratio 4 :years-history 1}})))

(defn eodhd-screen-page
  [_req]
  (let [cache* (atom {})
        tab* (h/tab-cursor :eodhd-screen :breakout)]
    (fn [req]
      (let [current (or (some #(when (= (:id %) @tab*) %) screens)
                        (first screens))
            loaded (or (get @cache* (:id current))
                       (let [v (try (load-screen (:id current))
                                    (catch Exception e
                                      {:error (or (ex-message e) (str e))}))]
                         (swap! cache* assoc (:id current) v)
                         v))]
        [:div.page
         (nav req)
         [:div.tabs
          (for [s screens]
            [:button {:key (name (:id s))
                      :type "button"
                      :class (str "tab" (when (= (:id s) (:id current)) " active"))
                      :data-on:click (h/action (reset! tab* (:id s)))}
             (:title s)])]
         [:h1 (:title current)]
         (if-let [err (:error loaded)]
           [:p.error err]
           [:div
            [:pre.screen-notes (str/join "\n" (:lines loaded))]
            (when (seq (:rows loaded))
              (table (assoc current :rows (:rows loaded))))])]))))
