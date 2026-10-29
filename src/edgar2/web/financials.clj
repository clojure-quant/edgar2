(ns edgar2.web.financials
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.filing :as filing]
            [edgar2.financials.facts-ui :as facts-ui]
            [edgar2.report.screen :as screen]
            [edgar2.report.stats-category :as stats-cat]
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
  "SIC code, then sector, industry, and subindustry."
  [ticker]
  (let [{:keys [sic sector industry subindustry sic-description]} (fsds/company ticker)
        code (some-> sic str str/trim not-empty)
        names (->> [sector industry subindustry]
                   (keep #(some-> % str str/trim not-empty)))
        desc (if (seq names)
               (str/join " · " names)
               (some-> sic-description str str/trim not-empty))]
    (when (or code desc)
      [:p.fin-sector
       (when code [:span.fin-sic code])
       (when (and code desc) " ")
       desc])))

(def sum-line
  "Last addend → the total drawn under the double rule.
  Gross profit closes revenue minus cost of revenue. Operating expenses
  closes SG&A + R&D + depreciation."
  {"Cost of Revenue" "Gross Profit"
   "Depreciation" "Operating Expenses"})

(defn- row-class
  "Background from the field `:format`, plus the double-rule borders."
  [field format rule? below?]
  (let [classes (cond-> []
                  rule? (conj "fin-above-rule")
                  (and below? (= field "Gross Profit")) (conj "fin-gross")
                  (and below? (= field "Operating Expenses")) (conj "fin-below-rule")
                  (= format :background-pl-green) (conj "fin-result")
                  (= format :background-balance-active) (conj "fin-asset")
                  (= format :background-balance-passive) (conj "fin-passive"))]
    (when (seq classes)
      (str/join " " classes))))

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
  "Ratio columns, grouped from `stats-category/categories`."
  (mapv (fn [cat]
          {:label (str/capitalize (name cat))
           :fields (mapv (fn [entry]
                           {:key (first entry)
                            :fmt (second entry)
                            :label (stats-cat/item-label entry)})
                         (get stats-cat/categories cat))})
        stats-cat/category-order))

(defn- ratio-cell
  [row {:keys [key fmt]}]
  (or (stats-cat/format-value fmt (get row key)) ""))

(defn- ratios-table
  [ticker]
  (let [row (get @stats-by-ticker (str/upper-case (str ticker)))]
    (if-not row
      [:p.fin-ratios-missing "No ratios in stats.edn."]
      [:div.fin-ratios
       (for [{:keys [label fields]} ratio-groups]
         [:div.fin-ratio-group {:key label}
          [:div.fin-ratio-group-label label]
          [:table
           [:thead
            [:tr
             [:th "Name"]
             [:th.num "Value"]]]
           [:tbody
            (for [field fields]
              [:tr {:key (str (:key field))}
               [:td (:label field)]
               [:td.num (ratio-cell row field)]])]]])])))

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
              ;; No next row makes both sides nil, which must not draw a rule.
              rule? (and (some? next-field)
                         (= (get sum-line field) next-field))
              below? (= (get sum-line prev-field) field)
              class (if (#{"year-end" "filing-date"} field)
                      "fin-year-end"
                      (row-class field (:format row) rule? below?))]
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

(defn- view-param
  "Path segment wins. `?view=` still works on a full page load."
  [req]
  (let [p (get-in req [:hyper/route :path-params])
        q (get-in req [:hyper/route :query-params])]
    (facts-ui/parse-view (or (:view p) (get p "view")
                             (:view q) (get q "view")))))

(defn- go-financials!
  "Open financials for the tab's ticker, count, period, and field view.

  Count, period, and view are path segments so a new choice remounts the
  page and refetches. Hyper does not remount on a query-param change.
  Annual with the current field list omits the period and view segments.
  `:n` is years when annual and quarters when quarterly. The ticker stays
  in the query."
  [n period view]
  (let [ticker (ticker/current)
        n (parse-n n)
        period (name (filing/parse-period period))
        view (facts-ui/parse-view view)
        q (when ticker {:ticker ticker})]
    (when (and ticker n)
      (cond
        (not= view :current)
        (effects/navigate! :company-financials-view
                           {:n (str n)
                            :period period
                            :view (name view)}
                           q)
        (= period "quarterly")
        (effects/navigate! :company-financials-period
                           {:n (str n)
                            :period "quarterly"}
                           q)
        :else
        (effects/navigate! :company-financials-n
                           {:n (str n)}
                           q)))))

(defn- period-form
  [n period view]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
     (go-financials! (:n $form-data) (:period $form-data) (:view $form-data)))}
   [:label "n "
    [:select {:name "n"
              :data-on:change
              (h/action
               (go-financials! (:n $form-data) (:period $form-data) (:view $form-data)))}
     (for [choice n-choices]
       [:option (cond-> {:key choice :value (str choice)}
                  (= choice n) (assoc :selected true))
        (str choice)])]]
   [:label "Period "
    [:select {:name "period"
              :data-on:change
              (h/action
               (go-financials! (:n $form-data) (:period $form-data) (:view $form-data)))}
     (for [[value label] period-choices]
       [:option (cond-> {:key value :value value}
                  (= value (name period)) (assoc :selected true))
        label])]]
   [:label "Fields "
    [:select {:name "view"
              :data-on:change
              (h/action
               (go-financials! (:n $form-data) (:period $form-data) (:view $form-data)))}
     (for [[value label] facts-ui/view-choices]
       [:option (cond-> {:key value :value (name value)}
                  (= value view) (assoc :selected true))
        label])]]
   [:button {:type "submit"} "Show"]])

(defn financials-page
  [req]
  (let [n (n-param req)
        period (period-param req)
        view (view-param req)]
    (h/view
      {:mount (fn []
                (ticker/seed! req)
                (ticker/begin
                  (fn [t]
                    (filing/financials-data {:ticker t
                                             :n n
                                             :period period
                                             :view view
                                             :inline-xbrl? true}))))
       :render (fn [{:keys [result*]} req]
                 (let [{:keys [status data message ticker]} @result*]
                   [:div.page
                    (nav req)
                    (period-form n period view)
                    (case status
                      :loading [:p (str "Loading " ticker " (n=" n ", "
                                        (name period) ", "
                                        (facts-ui/view-label view) ")…")]
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
