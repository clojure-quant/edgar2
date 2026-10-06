(ns edgar2.web.financials
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.filing :as filing]
            [edgar2.web.fsds :as fsds]
            [edgar2.web.nav :refer [nav]]))

(def year-choices
  [5 10 15 20 25 30 35 40 45 50])

(defn- ticker-param
  [req]
  (some-> (get-in req [:hyper/route :path-params :ticker])
          str
          str/trim
          not-empty
          str/upper-case))

(defn- parse-years
  "One of `year-choices`, or nil."
  [s]
  (let [n (try (Long/parseLong (str/trim (str s)))
               (catch Exception _ nil))]
    (when (some #{n} year-choices) n)))

(defn- years-param
  [req]
  (or (parse-years (get-in req [:hyper/route :path-params :years]))
      5))

(defn- cell
  [v]
  (if (nil? v) "" (str v)))

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

(defn- sector-line
  "SIC code and description, shown under the company name."
  [ticker]
  (let [{:keys [sic sic-description]} (fsds/company ticker)
        sector (str/trim (str/join " " (remove str/blank? [(str sic) sic-description])))]
    (when (seq sector)
      [:p.fin-header (str "Sector " sector)])))

(defn- filing-links
  "Google Finance and the cached annual-report link."
  [ticker form]
  (let [{:keys [exchange]} (fsds/company ticker)]
    [:div.fin-extra
     [:p
      [:a {:href (google-finance-url ticker exchange)
           :target "_blank"
           :rel "noopener noreferrer"}
       "Google Finance"]
      " · "
      [:a {:href (str "/filing/" ticker)}
       (str "Latest " (or form "annual report"))]]]))

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

(defn- go-financials!
  "Open financials for a ticker and year count. Years stay in the path so
  a new count remounts the page and refetches."
  [ticker years]
  (when (and ticker (parse-years years))
    (effects/navigate! :financials-years
                       {:ticker ticker
                        :years (str (parse-years years))})))

(defn- ticker-form
  [ticker years]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
     (go-financials! (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)
                     (:years $form-data)))}
   [:label "Ticker "
    [:input {:type "text"
             :name "ticker"
             :value (or ticker "")
             :autofocus true}]]
   [:label "Years "
    [:select {:name "years"
              :data-on:change
              (h/action
               (go-financials! (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)
                               (:years $form-data)))}
     (for [y year-choices]
       [:option (cond-> {:key y :value (str y)}
                  (= y years) (assoc :selected true))
        (str y)])]]
   [:button {:type "submit"} "Show"]])

(defn financials-page
  [req]
  (let [ticker (ticker-param req)
        years (years-param req)
        result* (atom (when ticker {:status :loading}))]
    (when ticker
      (h/watch! result*)
      (future
        (reset! result*
                (try
                  {:status :ready
                   :data (filing/financials-data {:ticker ticker :years years})}
                  (catch Throwable e
                    {:status :error
                     :message (or (ex-message e) (str e))})))))
    (fn [_req]
      (let [{:keys [status data message]} @result*]
        [:div.page
         (nav)
         (ticker-form ticker years)
         (case status
           :loading [:p (str "Loading " ticker " (" years " years)…")]
           :error [:p.error message]
           :ready [:div
                   [:p.fin-header
                    (format "%s  %s  CIK=%s  %s  ($ millions; EPS in $)"
                            (:ticker data) (:name data) (:cik data) (:form data))]
                   (sector-line ticker)
                   (financials-table data)
                   (filing-links ticker (:form data))]
           nil)]))))
