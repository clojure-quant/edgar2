(ns edgar2.web.financials
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.financials.facts-ui :as facts-ui]
            [edgar2.financials.facts-zip :as facts-zip]
            [edgar2.financials.view.core :as view]
            [edgar2.report.screen :as screen]
            [edgar2.report.stats-category :as stats-cat]
            [edgar2.util :as util]
            [edgar2.util.filing :as ufiling]
            [edgar2.util.ticker-info :as ticker-info]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]
            [tech.v3.dataset :as ds]))

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

(defn- stats-row
  "Row from data/stats.edn for `ticker`, or nil."
  [ticker]
  (get @stats-by-ticker (str/upper-case (str ticker))))

(defn- sector-line
  "SIC code, then sector, industry, and subindustry from the stats row."
  [ticker]
  (let [{:keys [sic sector industry subindustry]} (stats-row ticker)
        code (some-> sic str str/trim not-empty)
        names (->> [sector industry subindustry]
                   (keep #(some-> % str str/trim not-empty)))]
    (when (or code (seq names))
      [:p.fin-sector
       (when code [:span.fin-sic code])
       (when (and code (seq names)) " ")
       (when (seq names) (str/join " · " names))])))

(defn- header-text
  "Ticker, company name, and CIK from the stats row.
  Ticker only when that row is missing."
  [ticker]
  (let [row (stats-row ticker)
        name (some-> (:entityName row) str str/trim not-empty)
        cik (some-> (:cik row) str str/trim not-empty)
        ident (if row
                (->> [(str ticker) name (when cik (str "CIK=" cik))]
                     (keep identity)
                     (str/join "  "))
                (str ticker))]
    (str ident "  ($ millions; EPS in $)")))

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
  (let [row (stats-row ticker)]
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
  "Period columns stay yyyy-mm-dd so quarters in one year stay distinct."
  [col]
  (str col))

(def ^:private presentable-meta-cols
  #{:style :format :section})

(defn- financials-table
  "Render a presentable financials dataset. Hides `:style` / `:format` /
  `:section`. Missing style metadata skips row coloring (e.g. `:all`)."
  [table]
  (let [columns (into []
                      (remove #(contains? presentable-meta-cols %))
                      (ds/column-names table))
        rows (vec (ds/rows table {:nil-missing? true}))]
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
                style (or (:style row) (:format row))
                class (row-class field style rule? below?)]
            (cond-> [[:tr (cond-> {:key i}
                            class (assoc :class class))
                      (for [col columns]
                        [:td {:key (str col)
                              :class (when-not (= col :field) "num")}
                         (cell (get row col))])]]
              rule? (conj [:tr.fin-double-rule {:key (str "rule-" i)}
                           [:td {:colspan (count columns)}]]))))
        (map-indexed vector rows))]]]))

(def period-choices
  [["annual" "Annual"]
   ["quarterly" "Quarterly"]])

(defn- period-param
  [req]
  (ufiling/parse-period (get-in req [:hyper/route :path-params :period])))

(defn- view-param
  "Path segment wins. `?view=` still works on a full page load."
  [req]
  (let [p (get-in req [:hyper/route :path-params])
        q (get-in req [:hyper/route :query-params])]
    (view/parse-view (or (:view p) (get p "view")
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
        period (name (ufiling/parse-period period))
        view (view/parse-view view)
        q (when ticker {:ticker ticker})]
    (when (and ticker n)
      (cond
        (not= view :default)
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
     (for [[value label] view/view-choices]
       [:option (cond-> {:key value :value (name value)}
                  (= value view) (assoc :selected true))
        label])]]
   [:button {:type "submit"} "Show"]])

(defn- take-n-oldest-first
  "Keep the `n` newest period rows (table is newest-first), oldest first."
  [table n]
  (let [rc (ds/row-count table)
        n' (min (long n) rc)
        idxs (vec (range (dec n') -1 -1))]
    (if (zero? n')
      table
      (ds/select-rows table idxs))))

(defn- companyfacts-cik
  "10-digit CIK for the companyfacts zip entry.

  XOM uses the operating filer (`resolve-filer2`). Every other ticker
  uses the local exchange listing. `e/cik` would require an SEC identity."
  [ticker]
  (let [t (some-> ticker str str/trim not-empty str/upper-case)]
    (ticker-info/pad-cik
     (if (= t "XOM")
       (util/resolve-filer2 t)
       (ticker-info/ticker->cik t)))))

(defn- load-statement
  "Presentable statement for `ticker` from data/companyfacts.zip.

  `:n` is fiscal years when annual and quarters when quarterly."
  [ticker n period view]
  (let [cik (companyfacts-cik ticker)]
    (when-not cik
      (throw (ex-info (str "No CIK for " ticker) {:ticker ticker})))
    (let [dss (with-open [zf (facts-zip/open-facts-zip)]
                (facts-zip/load-dataset zf cik))]
      (when-not dss
        (throw (ex-info
                (format "No companyfacts for CIK %s in data/companyfacts.zip" cik)
                {:ticker ticker :cik cik})))
      (let [wide (facts-ui/statement dss
                                     (if (= period :quarterly) :q :y)
                                     view
                                     :standard)]
        {:ticker ticker
         :table (facts-ui/presentable (take-n-oldest-first wide n))}))))

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
                    (load-statement t n period view))))
       :render (fn [{:keys [result*]} req]
                 (let [{:keys [status data message ticker]} @result*]
                   [:div.page
                    (nav req)
                    (period-form n period view)
                    (case status
                      :loading [:p (str "Loading " ticker " (n=" n ", "
                                        (name period) ", "
                                        (view/view-label view) ")…")]
                      :error [:p.error message]
                      :ready [:div
                              [:p.fin-header (header-text ticker)]
                              (sector-line ticker)
                              (ratios-table ticker)
                              (financials-table (:table data))]
                      [:p "Enter a ticker."])]))
       :unmount (fn [{:keys [stop]}] (stop))})))
