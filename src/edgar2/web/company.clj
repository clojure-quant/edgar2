(ns edgar2.web.company
  "Google Finance, the latest annual report, and the company description."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.description :as description]
            [edgar2.web.fsds :as fsds]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]))

(def google-exchange
  {"NYSE" "NYSE"
   "Nasdaq" "NASDAQ"
   "OTC" "OTCMKTS"
   "CBOE" "CBOE"})

(defn google-finance-url
  [ticker exchange]
  (let [t (str/upper-case (str ticker))
        x (get google-exchange exchange)]
    (if x
      (str "https://www.google.com/finance/quote/" t ":" x)
      (str "https://www.google.com/finance/quote/" t))))

(defn- description-view
  [{:keys [text]}]
  (if-let [text (some-> text str/trim not-empty)]
    [:div.fin-description
     (for [[i para] (map-indexed vector (str/split text #"\n\n+"))]
       [:p {:key i} (str/trim para)])]
    [:p {:class "fin-description fin-description-loading"} "No company description."]))

(defn google-page
  [req]
  (ticker/seed! req)
  (fn [req]
    (let [t (ticker/current)]
      [:div.page
       (nav req)
       (if-not t
         [:p "Enter a ticker."]
         (let [{:keys [exchange]} (fsds/company t)
               url (google-finance-url t exchange)]
           [:p [:a {:href url
                    :target "_blank"
                    :rel "noopener noreferrer"}
                "Google Finance"]]))])))

(defn current-annual-page
  "Latest 10-K, 20-F, or 40-F, inside the company nav."
  [req]
  (ticker/seed! req)
  (fn [req]
    (let [t (ticker/current)]
      [:div.page
       (nav req)
       (if-not t
         [:p "Enter a ticker."]
         [:iframe.annual-frame {:src (str "/filing/" t)
                                :title "Latest annual report"}])])))

(defn description-page
  [req]
  (h/view
    {:mount (fn []
              (ticker/seed! req)
              (ticker/begin
                (fn [t]
                  (or (description/preferred t) {:text nil}))))
     :render (fn [{:keys [result*]} req]
               (let [{:keys [status ticker message] :as state} @result*]
                 [:div.page
                  (nav req)
                  (case status
                    :loading [:p {:class "fin-description fin-description-loading"}
                              (str "Loading description for " ticker "…")]
                    :error [:p.error message]
                    :ready (description-view (:data state))
                    [:p "Enter a ticker."])]))
     :unmount (fn [{:keys [stop]}] (stop))}))
