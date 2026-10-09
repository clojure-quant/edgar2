(ns edgar2.web.financials
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.filing :as filing]
            [edgar2.report.screen :as screen]
            [edgar2.web.fsds :as fsds]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]))

(def n-choices
  [5 10 15 20 25 30 35 40 45 50])

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

(def sum-line
  "Last addend → the total drawn under the double rule.
  Gross profit closes revenue minus cost of revenue. Operating expenses
  closes SG&A + R&D + depreciation."
  {"Cost of Revenue" "Gross Profit"
   "Depreciation" "Operating Expenses"})

(defn- row-class
  [field rule? below?]
  (cond
    rule? "fin-above-rule"
    (and (= field "Gross Profit") below?) "fin-result fin-gross"
    (= field "Gross Profit") "fin-result"
    (and below? (= field "Operating Expenses")) "fin-below-rule"
    (result-fields field) "fin-result"
    (asset-fields field) "fin-asset"
    (passive-fields field) "fin-passive"))

(def stats-by-ticker
  "ticker → row from data/stats.edn. Empty when that file is missing."
  (delay
    (try
      (into {}
            (map (fn [row]
                   [(str/upper-case (str (:ticker row))) row]))
            (screen/load-stats))
      (catch Throwable _
        {}))))

(def ratio-groups
  "Ratio columns, grouped. `:pe` is price-earnings.
  Market cap and enterprise value are millions of dollars.
  Shares are millions of shares."
  [{:label "Price"
    :fields [{:key :price :digits 2}
             {:key :shares :millions? true :label "shares (M)"}
             {:key :marketcap :millions? true}
             {:key :enterprise-value :millions? true}]}
   {:label "Quality"
    :fields [{:key :return-on-assets :digits 4}
             {:key :return-on-capital :digits 4}
             {:key :revenue-score :digits 1}
             {:key :operating-income-score :digits 1}
             {:key :profit-margin :digits 4}
             {:key :avg-profit-margin :digits 4}]}
   {:label "Growth"
    :fields [{:key :sales-growth-yoy :digits 1}]}
   {:label "Valuation"
    :fields [{:key :ev-ebit :digits 1}
             {:key :pe :source :price-earnings :digits 1}
             {:key :price-sales :digits 2}
             {:key :dividend-yield :digits 1}
             {:key :dividend-coverage :digits 1}]}])

(defn- ratio-label
  [{:keys [key label millions?]}]
  (or label
      (if millions? (str (name key) " ($M)") (name key))))

(defn- ratio-cell
  [row {:keys [key source digits millions?]}]
  (let [v (get row (or source key))]
    (cond
      (not (number? v)) ""
      millions? (str (Math/round (/ (double v) 1.0e6)))
      :else (format (str "%." digits "f") (double v)))))

(defn- ratios-table
  [ticker]
  (let [row (get @stats-by-ticker (str/upper-case (str ticker)))]
    (if-not row
      [:p.fin-ratios-missing "No ratios in stats.edn."]
      [:div.fin-ratios
       (for [{:keys [label fields]} ratio-groups]
         [:div.fin-ratio-group {:key label}
          [:div.fin-ratio-group-label label]
          [:div.fin-ratio-group-fields
           (for [field fields]
             [:div.fin-ratio {:key (ratio-label field)}
              [:div.fin-ratio-label (ratio-label field)]
              [:div.fin-ratio-value (ratio-cell row field)]])]])])))

(defn- col-heading
  "Annual columns are keyed by yyyy-mm-dd. The heading is the year."
  [col]
  (let [s (str col)]
    (if (re-matches #"\d{4}-\d{2}-\d{2}" s)
      (subs s 0 4)
      s)))

(defn- financials-table
  [{:keys [columns rows]}]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      (for [col columns]
        [:th {:key (str col)
              :class (when-not (= col :field) "num")}
         (col-heading col)])]]
    [:tbody
     (mapcat
      (fn [[i row]]
        (let [field (:field row)
              prev-field (:field (nth rows (dec i) nil))
              next-field (:field (nth rows (inc i) nil))
              ;; Two rules between the last addend and its total.
              rule? (= (get sum-line field) next-field)
              below? (= (get sum-line prev-field) field)
              class (if (= field "year-end")
                      "fin-year-end"
                      (row-class field rule? below?))]
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
  "Open financials for the tab's ticker, count, and period. The path includes
  the count and period so a new choice remounts the page and refetches.
  Annual omits the period segment. `:n` is years when annual and quarters
  when quarterly. The ticker stays in tab state."
  [n period]
  (let [ticker (ticker/current)
        n (parse-n n)
        period (name (filing/parse-period period))
        q (when ticker {:ticker ticker})]
    (when (and ticker n)
      (if (= period "quarterly")
        (effects/navigate! :company-financials-period
                           {:n (str n)
                            :period "quarterly"}
                           q)
        (effects/navigate! :company-financials-n
                           {:n (str n)}
                           q)))))

(defn- period-form
  [n period]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
     (go-financials! (:n $form-data) (:period $form-data)))}
   [:label "n "
    [:select {:name "n"
              :data-on:change
              (h/action
               (go-financials! (:n $form-data) (:period $form-data)))}
     (for [choice n-choices]
       [:option (cond-> {:key choice :value (str choice)}
                  (= choice n) (assoc :selected true))
        (str choice)])]]
   [:label "Period "
    [:select {:name "period"
              :data-on:change
              (h/action
               (go-financials! (:n $form-data) (:period $form-data)))}
     (for [[value label] period-choices]
       [:option (cond-> {:key value :value value}
                  (= value (name period)) (assoc :selected true))
        label])]]
   [:button {:type "submit"} "Show"]])

(defn financials-page
  [req]
  (let [n (n-param req)
        period (period-param req)]
    (h/view
      {:mount (fn []
                (ticker/seed! req)
                (ticker/begin
                  (fn [t]
                    (filing/financials-data {:ticker t
                                             :n n
                                             :period period}))))
       :render (fn [{:keys [result*]} req]
                 (let [{:keys [status data message ticker]} @result*]
                   [:div.page
                    (nav req)
                    (period-form n period)
                    (case status
                      :loading [:p (str "Loading " ticker " (n=" n ", " (name period) ")…")]
                      :error [:p.error message]
                      :ready [:div
                              [:p.fin-header
                               (format "%s  %s  CIK=%s  %s  ($ millions; EPS in $)"
                                       (:ticker data) (:name data) (:cik data) (:form data))]
                              (sector-line ticker)
                              (ratios-table ticker)
                              (financials-table data)]
                      [:p "Enter a ticker."])]))
       :unmount (fn [{:keys [stop]}] (stop))})))
