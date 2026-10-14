(ns edgar2.web.screen
  "One page for every screen. /screen and /eodhd-screen both render it."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.screen :as screen]
            [edgar2.screen.registry :as registry]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]))

(defn- cell [v]
  (if (nil? v) "" (str v)))

(defn- screen-table
  [{:keys [columns rows]}]
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

(defn- chosen-params
  "Parameter values for the screen, taken from the tab cursor or the default."
  [screen-map params*]
  (let [stored @params*]
    (into {}
          (map (fn [{:keys [key default]}]
                 [key (if (contains? stored key)
                        (get stored key)
                        default)])
               (:params screen-map)))))

(defn- param-select
  [param params* err*]
  (let [selected (let [stored (get @params* (:key param))]
                   (if (some? stored) stored (:default param)))
        parse (or (:parse param) identity)]
    [:label.screen-controls (str (:label param) " ")
     [:select {:data-on:change
               (h/action
                (reset! err* nil)
                (when-let [v (parse $value)]
                  (swap! params* assoc (:key param) v)))}
      (for [choice (:choices param)]
        [:option (cond-> {:key (str choice) :value (str choice)}
                   (= choice selected) (assoc :selected true))
         (str choice)])]]))

(defn screen-page
  [_req]
  (let [tab* (h/tab-cursor :screen :cheap-growth)
        params* (h/tab-cursor :screen-params {})
        err* (h/tab-cursor :screen-error nil)
        tick* (h/tab-cursor :screen-run 0)]
    (fn [req]
      (let [_tick @tick*
            err @err*
            screens (registry/all)
            current (or (some #(when (= (:id %) @tab*) %) screens)
                        (first screens))
            params (when current (chosen-params current params*))
            result (when (and current (not err))
                     (try (screen/cached-screen current params)
                          (catch Exception e
                            {:error (or (ex-message e) (str e))})))]
        [:div.page
         (nav req)
         [:div.tabs
          (for [s screens]
            [:button {:key (name (:id s))
                      :type "button"
                      :class (str "tab" (when (= (:id s) (:id current)) " active"))
                      :data-on:click (h/action
                                      (reset! err* nil)
                                      (reset! tab* (:id s)))}
             (:title s)])]
         (when current
           [:div
            (when (seq (:params current))
              [:div
               (for [param (:params current)]
                 [:span {:key (name (:key param))}
                  (param-select param params* err*)])])
            [:h1 (:title current)]
            (when-let [text (->> (concat (:description current) (:lines result))
                                 (remove str/blank?)
                                 seq)]
              [:pre.screen-notes (str/join "\n" text)])
            (when (:calculated-at result)
              [:p.screen-status (screen/status-line result)])
            [:button.tab {:type "button"
                          :data-on:click (h/action
                                          (try
                                            (screen/recalculate! (:id current) params)
                                            (reset! err* nil)
                                            (catch Exception e
                                              (reset! err* (or (ex-message e) (str e)))))
                                          (swap! tick* inc))}
             "Recalculate"]
            (cond
              err [:p.error err]
              (:error result) [:p.error (:error result)]
              (seq (:rows result)) (screen-table (assoc current :rows (:rows result))))])]))))
