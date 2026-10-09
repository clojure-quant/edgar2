(ns edgar2.web.filings
  "Filing index for one company, grouped by report category."
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.filing :as filing]
            [edgar2.web.nav :refer [nav]]))

(def default-category
  :quarterly)

(def categories
  "Display order. :other is the fallback in `classify`."
  [{:id :annual :label "annual reports"}
   {:id :quarterly :label "quarterly reports"}
   {:id :current :label "current reports"}
   {:id :proxy :label "proxy statements"}
   {:id :insider :label "insider transactions"}
   {:id :ownership :label "ownership"}
   {:id :registration :label "registration"}
   {:id :other :label "other"}])

(defn classify
  "Category id for an SEC form type. Amendments stay with the form they amend."
  [form]
  (let [form (str form)]
    (cond
      (str/starts-with? form "10-Q") :quarterly
      (or (str/starts-with? form "10-K")
          (str/starts-with? form "20-F")
          (str/starts-with? form "40-F")) :annual
      (or (str/starts-with? form "8-K")
          (str/starts-with? form "6-K")) :current
      (re-find #"14[AC]" form) :proxy
      (re-matches #"[345](/A)?" form) :insider
      (or (str/starts-with? form "SC 13")
          (str/starts-with? form "SCHEDULE 13")
          (str/starts-with? form "13F")) :ownership
      (or (re-find #"^(S|F)-\d+" form)
          (str/starts-with? form "424B")
          (str/starts-with? form "POS")
          (str/starts-with? form "EFFECT")) :registration
      :else :other)))

(defn- category-label
  [id]
  (if (= id :all)
    "all filings"
    (:label (some #(when (= id (:id %)) %) categories))))

(defn- query
  [req]
  (get-in req [:hyper/route :query-params]))

(defn- param
  [q k]
  (some-> (or (get q k) (get q (name k))) str str/trim not-empty))

(defn- ticker-param
  [q]
  (some-> (param q :ticker) str/upper-case))

(defn- date-span
  "Earliest and latest :filingDate, plus the row count."
  [filings]
  (let [dates (->> filings
                   (map :filingDate)
                   (filter #(re-matches #"\d{4}-\d{2}-\d{2}" (str %)))
                   sort)]
    {:n (count filings)
     :first (first dates)
     :last (last dates)}))

(defn summary-rows
  "Category totals. The first row is every filing. Quarterly reports is
  always present so the default selection has a row."
  [filings]
  (let [groups (group-by #(classify (:form %)) filings)
        row (fn [id label xs]
              (assoc (date-span xs) :id id :label label))]
    (into [(row :all "all filings" filings)]
          (keep (fn [{:keys [id label]}]
                  (let [xs (get groups id)]
                    (when (or (seq xs) (= id :quarterly))
                      (row id label (or xs []))))))
          categories)))

(defn- selected-filings
  [filings cat]
  (if (= cat :all)
    filings
    (filter #(= cat (classify (:form %))) filings)))

(defn- by-date-desc
  [filings]
  (sort-by #(str (:filingDate %)) #(compare %2 %1) filings))

(defn load-filings
  "Filings for a ticker or a CIK. A ticker wins when both are set.
  Tickers go through the same CIK resolution as the financials page."
  [ticker cik]
  (filing/ensure-identity!)
  (let [id (if ticker
             (filing/resolve-filer ticker)
             (e/cik cik))
        filings (vec (e/filings id :include-amends? true))
        company (e/company id)]
    {:cik (e/cik id)
     :ticker (or ticker
                  (some-> (:tickers company) first str/upper-case))
     :name (:name company)
     :filings filings}))

(defn- go-filings!
  "Open /filings with one query param. A typed ticker wins over a CIK."
  [ticker cik]
  (cond
    ticker (effects/navigate! :filings {} {:ticker ticker})
    cik (effects/navigate! :filings {} {:cik cik})))

(defn- lookup-form
  [ticker cik]
  [:form.ticker-form
   {:data-on:submit__prevent
    (h/action
     (go-filings! (some-> (:ticker $form-data) str str/trim not-empty str/upper-case)
                  (some-> (:cik $form-data) str str/trim not-empty)))}
   [:label "Ticker "
    [:input {:type "text"
             :name "ticker"
             :value (or ticker "")
             :autofocus true}]]
   [:label "CIK "
    [:input {:type "text"
             :name "cik"
             :value (or cik "")}]]
   [:button {:type "submit"} "Show"]])

(defn- summary-table
  [rows selected cat*]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      [:th "category"]
      [:th.num "number of filings"]
      [:th "first filing date"]
      [:th "last filing date"]]]
    [:tbody
     (for [{:keys [id label n first last]} rows]
       [:tr {:key (name id)
             :class (str "filings-cat" (when (= id selected) " selected"))
             :data-on:click (h/action (reset! cat* id))}
        [:td label]
        [:td.num (format "%,d" n)]
        [:td (or first "")]
        [:td (or last "")]])]]])

(defn- filings-table
  [cik filings]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      [:th "category"]
      [:th "date"]
      [:th "OPEN"]]]
    [:tbody
     (for [f (by-date-desc filings)]
       [:tr {:key (or (:accessionNumber f) (str (:form f) (:filingDate f)))}
        [:td (str (:form f))]
        [:td (or (:filingDate f) "")]
        [:td
         (when (:accessionNumber f)
           [:a {:href (str "/filing/" cik "/" (:accessionNumber f)
                           (when-let [doc (:primaryDocument f)]
                             (str "/" doc)))
                :target "_blank"
                :rel "noopener noreferrer"}
            "OPEN"])]])]]])

(defn filings-page
  "Company filings. Query param is `ticker` or `cik`."
  [req]
  (let [q (query req)
        ticker (ticker-param q)
        cik (param q :cik)
        result* (atom (when (or ticker cik) {:status :loading}))
        cat* (atom default-category)]
    (when (or ticker cik)
      (h/watch! result*)
      (h/watch! cat*)
      (future
        (reset! result*
                (try
                  {:status :ready
                   :data (load-filings ticker cik)}
                  (catch Throwable e
                    {:status :error
                     :message (or (ex-message e) (str e))})))))
    (fn [_req]
      (let [{:keys [status data message]} @result*
            selected @cat*
            filings (:filings data)]
        [:div.page
         (nav)
         [:h1 "Filings"]
         (lookup-form ticker cik)
         (case status
           :loading [:p (str "Loading filings for " (or ticker cik) "…")]
           :error [:p.error message]
           :ready [:div
                   [:p.fin-header
                    (format "%s  %s  CIK=%s"
                            (or (:ticker data) "")
                            (or (:name data) "")
                            (:cik data))]
                   (summary-table (summary-rows filings) selected cat*)
                   [:div.filings-detail
                    [:h2 (category-label selected)]
                    (let [rows (selected-filings filings selected)]
                      (if (seq rows)
                        (filings-table (:cik data) rows)
                        [:p "No filings."]))]]
           [:p "Enter a ticker or CIK."])]))))
