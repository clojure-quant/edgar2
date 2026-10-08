(ns edgar2.web.segment
  "Companies behind one SIC description or one stats metric."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.report.screen :as screen]
            [edgar2.report.stats :as stats]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.overview :as overview]))

(def columns
  [:ticker :entityName :price :mc :ev :pe :ev-ebit])

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
  "Rows for a SIC description, or every company with a number for `metric`."
  [rows {:keys [sic metric]}]
  (cond
    sic (->> rows
             (filter #(= sic (some-> (:sic-description %) str str/trim)))
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
              [:a (h/navigate :financials {:ticker ticker}) (str ticker)]
              "")
    :entityName (str (:entityName row ""))
    :price (or (fmt (:price row) 2) "")
    :mc (if (number? (:marketcap row)) (str (screen/millions (:marketcap row))) "")
    :ev (if (number? (:enterprise-value row)) (str (screen/millions (:enterprise-value row))) "")
    :pe (or (some-> (:price-earnings row) stats/round1 str) "")
    :ev-ebit (or (some-> (:ev-ebit row) stats/round1 str) "")))

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

(defn segment-page
  [req]
  (let [q (query req)
        sic (param q :sic)
        metric-name (param q :metric)
        spec (metric metric-name)
        loaded (try {:rows (overview/load-rows)}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))]
    [:div.page
     (nav)
     (cond
       (:error loaded) [:p.error (:error loaded)]
       sic (let [rows (vec (companies (:rows loaded) {:sic sic}))]
             [:div
              [:h1 sic]
              [:p.quintile-note (format "%s companies" (format "%,d" (count rows)))]
              (company-table rows)])
       spec (let [rows (vec (companies (:rows loaded) {:metric (:key spec)}))]
              [:div
               [:h1 (:label spec)]
               [:p.quintile-note (format "%s companies" (format "%,d" (count rows)))]
               (company-table rows)])
       metric-name [:p.error (str "Unknown metric " metric-name)]
       :else [:p "Open a SIC description or a metric from the stats page."])]))
