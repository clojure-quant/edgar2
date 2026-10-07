(ns edgar2.web.screen
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.report.screen :as screen]
            [edgar2.web.nav :refer [nav]]))

(defn- cell
  [v]
  (if (nil? v) "" (str v)))

(defn- screen-table
  [{:keys [columns rows]}]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      (for [col columns]
        [:th {:key (str col)
              :class (when-not (#{:ticker :entityName} col) "num")}
         (str col)])]]
    [:tbody
     (for [[i row] (map-indexed vector rows)]
       [:tr {:key i}
        (for [col columns]
          [:td {:key (str col)
                :class (when-not (#{:ticker :entityName} col) "num")}
           (if (= col :ticker)
             (if-let [ticker (:ticker row)]
               [:a (h/navigate :financials {:ticker ticker}) (str ticker)]
               "")
             (cell (get row col)))])])]]])

(defn- mc-select
  [mc*]
  (let [selected (or (screen/parse-min-mc @mc*) screen/default-min-mc)]
    [:label.screen-controls "Min mc ($M) "
     [:select {:data-on:change
               (h/action
                (when-let [n (screen/parse-min-mc $value)]
                  (reset! mc* n)))}
      (for [choice screen/mc-choices]
        [:option (cond-> {:key choice :value (str choice)}
                   (= choice selected) (assoc :selected true))
         (str choice)])]]))

(defn screen-page
  [_req]
  (let [loaded (try {:rows (screen/load-stats)}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))
        cache* (atom {})
        tab* (h/tab-cursor :screen :cheap-growth)
        mc* (h/tab-cursor :min-mc screen/default-min-mc)]
    (fn [_req]
      (let [min-mc (or (screen/parse-min-mc @mc*) screen/default-min-mc)
            specs (when (:rows loaded)
                    (or (get @cache* min-mc)
                        (let [s (screen/screen-specs (:rows loaded) {:min-mc min-mc})]
                          (swap! cache* assoc min-mc s)
                          s)))
            current (or (some #(when (= (:id %) @tab*) %) specs)
                        (first specs))]
        [:div.page
         (nav)
         (if-let [err (:error loaded)]
           [:p.error err]
           [:div
            (mc-select mc*)
            [:div.tabs
             (for [s specs]
               [:button {:key (name (:id s))
                         :type "button"
                         :class (str "tab" (when (= (:id s) (:id current)) " active"))
                         :data-on:click (h/action (reset! tab* (:id s)))}
                (:title s)])]
            [:h1 (:title current)]
            [:pre.screen-notes (str/join "\n" (:lines current))]
            (when (:table? current)
              (screen-table current))])]))))
