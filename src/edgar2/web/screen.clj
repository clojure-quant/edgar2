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

(defn screen-page
  [_req]
  (let [loaded (try {:specs (screen/screen-specs (screen/load-stats))}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))
        tab* (h/tab-cursor :screen :cheap-growth)]
    (fn [_req]
      (let [specs (:specs loaded)
            current (or (some #(when (= (:id %) @tab*) %) specs)
                        (first specs))]
        [:div.page
         (nav)
         (if-let [err (:error loaded)]
           [:p.error err]
           [:div
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
