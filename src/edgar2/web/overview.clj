(ns edgar2.web.overview
  "Universe quintiles from data/stats.edn."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.report.screen :as screen]
            [edgar2.report.stats :as stats]
            [edgar2.web.nav :refer [nav]]))

(def groups
  [{:title "SEGMENT"
    :metrics [{:key :marketcap :label "marketcap ($M)" :fmt :millions}]}
   {:title "QUALITY"
    :metrics [{:key :return-on-capital :label "return-on-capital (%)" :fmt :percent}
              {:key :revenue-score :label "revenue-score" :fmt :number}
              {:key :operating-income-score :label "operating-income-score" :fmt :number}
              {:key :avg-profit-margin :label "avg-profit-margin (%)" :fmt :percent}]}
   {:title "GROWTH"
    :metrics [{:key :sales-growth-yoy :label "sales-growth-yoy (%)" :fmt :number}]}
   {:title "VALUATION"
    :metrics [{:key :ev-ebit :label "ev-ebit" :fmt :number}
              {:key :price-earnings :label "pe" :fmt :number}
              {:key :price-sales :label "price-sales" :fmt :number}
              {:key :dividend-yield :label "dividend-yield (%)" :fmt :number}]}])

(defonce ^:private cache* (atom {:mtime nil :rows nil}))

(defn quantile
  "Nearest-rank value at fraction p (0 < p <= 1) of an ascending vector."
  [sorted p]
  (let [n (count sorted)
        idx (min (dec n) (max 0 (dec (int (Math/ceil (* (double p) n))))))]
    (nth sorted idx)))

(defn quintile-cuts
  "Tops of Q1–Q4 (20th, 40th, 60th, 80th percentile). Empty when xs is empty."
  [xs]
  (when (seq xs)
    (let [sorted (vec (sort xs))]
      (mapv #(quantile sorted %) [0.2 0.4 0.6 0.8]))))

(defn- format-value
  [fmt x]
  (case fmt
    :millions (str (long (Math/round (/ (double x) 1.0e6))))
    :percent (format "%.1f" (* 100.0 (double x)))
    :number (format "%.1f" (double x))))

(defn- load-rows
  []
  (let [f (io/file stats/stats-path)]
    (when-not (.exists f)
      (throw (ex-info "Missing stats file; run clj -X:stats first"
                      {:path stats/stats-path})))
    (let [mtime (.lastModified f)
          hit @cache*]
      (if (and (:rows hit) (= mtime (:mtime hit)))
        (:rows hit)
        (let [rows (screen/load-stats)]
          (reset! cache* {:mtime mtime :rows rows})
          rows)))))

(defn- metric-row
  [rows {:keys [label key fmt]}]
  (let [xs (filterv number? (keep key rows))
        [q1 q2 q3 q4] (quintile-cuts xs)
        cell (fn [k x] [:td.num {:key k} (if x (format-value fmt x) "")])]
    [:tr {:key label}
     [:td label]
     [:td.num (count xs)]
     (cell :q1 q1)
     (cell :q2 q2)
     (cell :q3 q3)
     (cell :q4 q4)
     [:td.num {:key :q5} (if q4 (str "> " (format-value fmt q4)) "")]]))

(defn sic-summary
  "One row per :sic-description. :n is the company count. :avg-mkt-cap is
  the mean :marketcap of companies in that description that have one."
  [rows]
  (->> rows
       (filter #(not-empty (some-> (:sic-description %) str str/trim)))
       (group-by #(str/trim (str (:sic-description %))))
       (map (fn [[desc xs]]
              (let [mcaps (filterv number? (keep :marketcap xs))]
                {:sic-description desc
                 :n (count xs)
                 :avg-mkt-cap (when (seq mcaps)
                                (/ (reduce + (map double mcaps)) (count mcaps)))})))
       (sort-by (juxt (comp - :n) :sic-description))
       vec))

(defn- sic-table
  [rows]
  (let [groups (sic-summary rows)]
    [:div.table-wrap
     [:table
      [:thead
       [:tr
        [:th "sic-description"]
        [:th.num "n"]
        [:th.num "avg-mkt-cap ($M)"]]]
      (into [:tbody]
            (map (fn [{:keys [sic-description n avg-mkt-cap]}]
                   [:tr {:key sic-description}
                    [:td sic-description]
                    [:td.num n]
                    [:td.num (if avg-mkt-cap
                               (format-value :millions avg-mkt-cap)
                               "")]]))
            groups)]]))

(defn- quintile-table
  [rows]
  [:div.table-wrap
   [:table
    [:thead
     [:tr
      [:th "metric"]
      [:th.num "n"]
      [:th.num "Q1"]
      [:th.num "Q2"]
      [:th.num "Q3"]
      [:th.num "Q4"]
      [:th.num "Q5"]]]
    (into [:tbody]
          (mapcat (fn [{:keys [title metrics]}]
                    (cons [:tr.quintile-group {:key title}
                           [:th {:colspan 7} title]]
                          (map #(metric-row rows %) metrics)))
                  groups))]])

(defn overview-page
  [_req]
  (let [loaded (try {:rows (load-rows)}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))]
    [:div.page
     (nav)
     [:h1 "Stats"]
     (if-let [err (:error loaded)]
       [:p.error err]
       [:div
        [:p.quintile-note
         (format "%s companies. Q1–Q4 are the top of each quintile; Q5 is above Q4."
                 (format "%,d" (count (:rows loaded))))]
        (quintile-table (:rows loaded))
        [:h2.stats-section "SIC"]
        (sic-table (:rows loaded))])]))
