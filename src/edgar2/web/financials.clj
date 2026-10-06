(ns edgar2.web.financials
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.filing :as filing]
            [edgar2.web.nav :refer [nav]]))

(defn- ticker-param
  [req]
  (some-> (get-in req [:hyper/route :path-params :ticker])
          str
          str/trim
          not-empty
          str/upper-case))

(defn- cell
  [v]
  (if (nil? v) "" (str v)))

(defn- financials-table
  [{:keys [columns rows]}]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      (for [col columns]
        [:th {:key (str col)
              :class (when-not (= col :field) "num")}
         (str col)])]]
    [:tbody
     (for [[i row] (map-indexed vector rows)]
       [:tr {:key i}
        (for [col columns]
          [:td {:key (str col)
                :class (when-not (= col :field) "num")}
           (cell (get row col))])])]]])

(defn- ticker-form
  [ticker]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
     (let [t (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)]
       (when t
         (effects/navigate! :financials {:ticker t}))))}
   [:label "Ticker "
    [:input {:type "text"
             :name "ticker"
             :value (or ticker "")
             :autofocus true}]]
   [:button {:type "submit"} "Show"]])

(defn financials-page
  [req]
  (let [ticker (ticker-param req)
        result* (atom (when ticker {:status :loading}))]
    (when ticker
      (h/watch! result*)
      (future
        (reset! result*
                (try
                  {:status :ready
                   :data (filing/financials-data {:ticker ticker})}
                  (catch Throwable e
                    {:status :error
                     :message (or (ex-message e) (str e))})))))
    (fn [_req]
      (let [{:keys [status data message]} @result*]
        [:div.page
         (nav)
         (ticker-form ticker)
         (case status
           :loading [:p (str "Loading " ticker "…")]
           :error [:p.error message]
           :ready [:div
                   [:p.fin-header
                    (format "%s  %s  CIK=%s  %s  ($ millions; EPS in $)"
                            (:ticker data) (:name data) (:cik data) (:form data))]
                   (financials-table data)]
           nil)]))))
