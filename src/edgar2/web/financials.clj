(ns edgar2.web.financials
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.filing :as filing]
            [edgar2.web.fsds :as fsds]
            [edgar2.web.nav :refer [nav]]))

(def n-choices
  [5 10 15 20 25 30 35 40 45 50])

(defn- ticker-param
  [req]
  (some-> (get-in req [:hyper/route :path-params :ticker])
          str
          str/trim
          not-empty
          str/upper-case))

(defn- parse-n
  "One of `n-choices`, or nil."
  [s]
  (let [n (try (Long/parseLong (str/trim (str s)))
               (catch Exception _ nil))]
    (when (some #{n} n-choices) n)))

(defn- n-param
  [req]
  (or (parse-n (get-in req [:hyper/route :path-params :n]))
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
  "SIC code, boxed, then the description."
  [ticker]
  (let [{:keys [sic sic-description]} (fsds/company ticker)
        code (some-> sic str str/trim not-empty)
        desc (some-> sic-description str str/trim not-empty)]
    (when (or code desc)
      [:p.fin-sector
       (when code [:span.fin-sic code])
       (when (and code desc) " ")
       desc])))

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

(def result-fields
  "Subtotals drawn with a light green row."
  #{"Gross Profit" "Operating Income" "Net Income"})

(def asset-fields
  "Cash and equivalents through total assets."
  #{"Cash and Equivalents"
    "Accounts Receivable"
    "Inventory"
    "Current Assets"
    "PP&E Net"
    "Total Assets"})

(def passive-fields
  "Accounts payable through total liabilities and equity."
  #{"Accounts Payable"
    "Current Debt"
    "Current Portion of Long-Term Debt"
    "Short-Term Borrowings"
    "Current Liabilities"
    "Long-Term Debt"
    "Total Liabilities"
    "Stockholders Equity"
    "Total Equity"
    "Total Liabilities and Equity"})

(defn- row-class
  [field rule?]
  (cond
    rule? "fin-above-rule"
    (= field "Gross Profit") "fin-result fin-gross"
    (result-fields field) "fin-result"
    (asset-fields field) "fin-asset"
    (passive-fields field) "fin-passive"))

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
     (mapcat
      (fn [[i row]]
        (let [field (:field row)
              next-field (:field (nth rows (inc i) nil))
              ;; Revenue − cost of revenue. Two rules sit between those rows.
              rule? (and (= field "Cost of Revenue") (= next-field "Gross Profit"))
              class (row-class field rule?)]
          (cond-> [[:tr (cond-> {:key i}
                          class (assoc :class class))
                    (for [col columns]
                      [:td {:key (str col)
                            :class (when-not (= col :field) "num")}
                       (cell (get row col))])]]
            rule? (conj [:tr.fin-double-rule {:key (str "rule-" i)}
                         [:td {:colspan (count columns)}]]))))
      (map-indexed vector rows))]]])

(def period-choices
  [["annual" "Annual"]
   ["quarterly" "Quarterly"]])

(defn- period-param
  [req]
  (filing/parse-period (get-in req [:hyper/route :path-params :period])))

(defn- go-financials!
  "Open financials for a ticker, count, and period. The path includes both
  so a new choice remounts the page and refetches. Annual omits the period
  segment. `:n` is years when annual and quarters when quarterly."
  [ticker n period]
  (let [n (parse-n n)
        period (name (filing/parse-period period))]
    (when (and ticker n)
      (if (= period "quarterly")
        (effects/navigate! :financials-period
                           {:ticker ticker
                            :n (str n)
                            :period "quarterly"})
        (effects/navigate! :financials-n
                           {:ticker ticker
                            :n (str n)})))))

(defn- ticker-form
  [ticker n period]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
     (go-financials! (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)
                     (:n $form-data)
                     (:period $form-data)))}
   [:label "Ticker "
    [:input {:type "text"
             :name "ticker"
             :value (or ticker "")
             :autofocus true}]]
   [:label "n "
    [:select {:name "n"
              :data-on:change
              (h/action
               (go-financials! (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)
                               (:n $form-data)
                               (:period $form-data)))}
     (for [choice n-choices]
       [:option (cond-> {:key choice :value (str choice)}
                  (= choice n) (assoc :selected true))
        (str choice)])]]
   [:label "Period "
    [:select {:name "period"
              :data-on:change
              (h/action
               (go-financials! (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)
                               (:n $form-data)
                               (:period $form-data)))}
     (for [[value label] period-choices]
       [:option (cond-> {:key value :value value}
                  (= value (name period)) (assoc :selected true))
        label])]]
   [:button {:type "submit"} "Show"]])

(defn financials-page
  [req]
  (let [ticker (ticker-param req)
        n (n-param req)
        period (period-param req)
        result* (atom (when ticker {:status :loading}))]
    (when ticker
      (h/watch! result*)
      (future
        (reset! result*
                (try
                  {:status :ready
                   :data (filing/financials-data {:ticker ticker
                                                  :n n
                                                  :period period})}
                  (catch Throwable e
                    {:status :error
                     :message (or (ex-message e) (str e))})))))
    (fn [_req]
      (let [{:keys [status data message]} @result*]
        [:div.page
         (nav)
         (ticker-form ticker n period)
         (case status
           :loading [:p (str "Loading " ticker " (n=" n ", " (name period) ")…")]
           :error [:p.error message]
           :ready [:div
                   [:p.fin-header
                    (format "%s  %s  CIK=%s  %s  ($ millions; EPS in $)"
                            (:ticker data) (:name data) (:cik data) (:form data))]
                   (sector-line ticker)
                   (financials-table data)
                   (filing-links ticker (:form data))]
           nil)]))))
