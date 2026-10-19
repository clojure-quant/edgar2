(ns edgar2.web.segment
  "Companies behind one SIC description, one stats metric, or one reporting currency."
  (:require [clojure.string :as str]
            [edgar2.report.screen :as screen]
            [edgar2.report.stats :as stats]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.ticker :as ticker]
            [edgar2.web.overview :as overview]))

(def columns
  [:ticker :entityName :price :mc :ev :pe :ev-ebit])

(def foreign-columns
  [:ticker :reporting-cur :adr-ratio :market-cap-adjusted])

(defn- query
  [req]
  (get-in req [:hyper/route :query-params]))

(defn- param
  [q k]
  (some-> (or (get q k) (get q (name k))) str str/trim not-empty))

(defn- metric
  [name]
  (when name
    (let [k (keyword name)]
      (some #(when (= k (:key %)) %)
            (mapcat :metrics overview/groups)))))

(defn companies
  "Rows for a SIC description, a reporting currency, or every company with a
  number for `metric`."
  [rows {:keys [sic cur metric]}]
  (cond
    sic (->> rows
             (filter #(= sic (overview/sic-label %)))
             (sort-by (fn [r]
                        [(if (number? (:marketcap r)) 0 1)
                         (- (double (or (:marketcap r) 0)))
                         (str (:ticker r))])))
    cur (->> rows
             (filter #(= (str/upper-case cur) (overview/reporting-cur %)))
             (sort-by (fn [r]
                        [(if (number? (:marketcap r)) 0 1)
                         (- (double (or (:marketcap r) 0)))
                         (str (:ticker r))])))
    metric (->> rows
                (filter (fn [r] (number? (metric r))))
                (sort-by (fn [r] [(- (double (metric r))) (str (:ticker r))])))
    :else []))

(defn- fmt
  [x digits]
  (when (number? x)
    (format (str "%." digits "f") (double x))))

(defn- cell
  [col row]
  (case col
    :ticker (if-let [ticker (:ticker row)]
              [:a (ticker/company-link ticker) (str ticker)]
              "")
    :entityName (str (:entityName row ""))
    :price (or (fmt (:price row) 2) "")
    :mc (if (number? (:marketcap row)) (str (screen/millions (:marketcap row))) "")
    :ev (if (number? (:enterprise-value row)) (str (screen/millions (:enterprise-value row))) "")
    :pe (or (some-> (:price-earnings row) stats/round1 str) "")
    :ev-ebit (or (some-> (:ev-ebit row) stats/round1 str) "")))

(defn- foreign-cell
  [col row]
  (case col
    :ticker (if-let [ticker (:ticker row)]
              [:a (ticker/company-link ticker) (str ticker)]
              "")
    :reporting-cur (or (overview/reporting-cur row) "")
    :adr-ratio (or (fmt (:adr-ratio row) 2) "")
    :market-cap-adjusted
    (if-let [adj (overview/market-cap-adjusted row)]
      (str (screen/millions adj))
      "")))

(defn- company-table
  [rows]
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
       [:tr {:key (or (:ticker row) i)}
        (for [col columns]
          [:td {:key (str col)
                :class (when-not (#{:ticker :entityName} col) "num")}
           (cell col row)])])]]])

(defn- foreign-company-table
  [rows]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      (for [col foreign-columns]
        [:th {:key (str col)
              :class (when-not (#{:ticker :reporting-cur} col) "num")}
         (str col)])]]
    [:tbody
     (for [[i row] (map-indexed vector rows)]
       [:tr {:key (or (:ticker row) i)}
        (for [col foreign-columns]
          [:td {:key (str col)
                :class (when-not (#{:ticker :reporting-cur} col) "num")}
           (foreign-cell col row)])])]]])

(defn segment-page
  [req]
  (let [q (query req)
        sic (param q :sic)
        cur (some-> (param q :cur) str/upper-case)
        metric-name (param q :metric)
        spec (metric metric-name)
        loaded (try {:rows (overview/load-rows)}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))]
    [:div.page
     (nav req)
     (cond
       (:error loaded) [:p.error (:error loaded)]
       sic (let [rows (vec (companies (:rows loaded) {:sic sic}))]
             [:div
              [:h1 sic]
              [:p.quintile-note (format "%s companies" (format "%,d" (count rows)))]
              (company-table rows)])
       cur (let [rows (vec (companies (:rows loaded) {:cur cur}))]
             [:div
              [:h1 cur]
              [:p.quintile-note
               (format "%s companies. market-cap-adjusted is in %s (millions)."
                       (format "%,d" (count rows))
                       cur)]
              (foreign-company-table rows)])
       spec (let [rows (vec (companies (:rows loaded) {:metric (:key spec)}))]
              [:div
               [:h1 (:label spec)]
               [:p.quintile-note (format "%s companies" (format "%,d" (count rows)))]
               (company-table rows)])
       metric-name [:p.error (str "Unknown metric " metric-name)]
       :else [:p "Open a SIC description, a reporting currency, or a metric from the stats page."])]))
