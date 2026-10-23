(ns edgar2.web.overview
  "Universe percentiles from data/stats.edn."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.report.screen :as screen]
            [edgar2.report.stats :as stats]
            [edgar2.web.nav :refer [nav]]))

(def groups
  [{:title "SEGMENT"
    :metrics [{:key :marketcap :label "marketcap ($M)" :fmt :millions}
              {:key :annual-count :label "annual-report-count" :fmt :integer}]}
   {:title "QUALITY"
    :metrics [{:key :return-on-capital :label "return-on-capital (%)" :fmt :percent}
              {:key :revenue-score :label "revenue-score" :fmt :number}
              {:key :operating-income-score :label "operating-income-score" :fmt :number}
              {:key :avg-profit-margin :label "avg-profit-margin (%)" :fmt :percent}
              {:key :debt2ebitda :label "debt2ebitda" :fmt :number}]}
   {:title "GROWTH"
    :metrics [{:key :sales-growth-yoy :label "sales-growth-yoy (%)" :fmt :number}
              {:key :net-income-change-q-on-y :label "net-income-change-q-on-y (%)" :fmt :number}]}
   {:title "VALUATION"
    :metrics [{:key :ev-ebit :label "ev-ebit" :fmt :number}
              {:key :price-earnings :label "pe" :fmt :number}
              {:key :price-sales :label "price-sales" :fmt :number}
              {:key :dividend-yield :label "dividend-yield (%)" :fmt :number}
              {:key :buyback-yield :label "buyback-yield (%)" :fmt :number}]}])

(def percentile-count
  "Number of equal-sized bands on the overview / segment pages."
  10)

(defonce ^:private cache* (atom {:mtime nil :rows nil}))

(defn quantile
  "Nearest-rank value at fraction p (0 < p <= 1) of an ascending vector."
  [sorted p]
  (let [n (count sorted)
        idx (min (dec n) (max 0 (dec (int (Math/ceil (* (double p) n))))))]
    (nth sorted idx)))

(defn percentile-cuts
  "Tops of P1–P9 (10th…90th percentile). Empty when xs is empty."
  [xs]
  (when (seq xs)
    (let [sorted (vec (sort xs))
          fracs (mapv #(/ (double %) percentile-count)
                      (range 1 percentile-count))]
      (mapv #(quantile sorted %) fracs))))

(defn parse-percentile
  "1-based percentile band from a query string, or nil."
  [s]
  (when-let [n (try (Long/parseLong (str s))
                    (catch Exception _ nil))]
    (when (<= 1 n percentile-count) n)))

(defn in-percentile?
  "True when `v` falls in 1-based band `p` (1..10) of the distribution `xs`.

  P1 is at or below the 10th percentile cut; P10 is above the 90th.
  Intermediate bands are (prev-cut, cut]."
  [xs p v]
  (let [cuts (percentile-cuts xs)
        p (long p)]
    (boolean
     (when (and (seq cuts) (number? v) (<= 1 p percentile-count))
       (let [v (double v)
             lo (when (> p 1) (double (nth cuts (- p 2))))
             hi (when (< p percentile-count) (double (nth cuts (dec p))))]
         (cond
           (= p 1) (<= v hi)
           (= p percentile-count) (> v lo)
           :else (and (> v lo) (<= v hi))))))))

(defn- format-value
  [fmt x]
  (case fmt
    :millions (str (long (Math/round (/ (double x) 1.0e6))))
    :percent (format "%.1f" (* 100.0 (double x)))
    :integer (str (long (Math/round (double x))))
    :number (format "%.1f" (double x))))

(defn load-rows
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

(defn- percentile-link
  [metric-key p text]
  [:a (h/navigate :segment {} {:metric (name metric-key)
                               :percentile (str p)})
   text])

(defn- metric-row
  [rows {:keys [label key fmt]}]
  (let [xs (filterv number? (keep key rows))
        cuts (percentile-cuts xs)
        last-cut (when (seq cuts) (last cuts))]
    (into [:tr {:key label}
           [:td [:a (h/navigate :segment {} {:metric (name key)}) label]]
           [:td.num (count xs)]]
          (concat
           (map (fn [i cut]
                  (let [p (inc i)]
                    [:td.num {:key p}
                     (when cut
                       (percentile-link key p (format-value fmt cut)))]))
                (range (count cuts))
                (or cuts []))
           [[:td.num {:key percentile-count}
             (when last-cut
               (percentile-link key percentile-count
                                (str "> " (format-value fmt last-cut))))]]))))

(defn sic-label
  "Industry name, or the older :sic-description when that is all a row has."
  [row]
  (or (some-> (:industry row) str str/trim not-empty)
      (some-> (:sic-description row) str str/trim not-empty)))

(defn sic-summary
  "One row per industry. :n is the company count. :avg-mkt-cap is
  the mean :marketcap of companies in that industry that have one."
  [rows]
  (->> rows
       (filter #(sic-label %))
       (group-by sic-label)
       (map (fn [[desc xs]]
              (let [mcaps (filterv number? (keep :marketcap xs))]
                {:industry desc
                 :n (count xs)
                 :avg-mkt-cap (when (seq mcaps)
                                (/ (reduce + (map double mcaps)) (count mcaps)))})))
       (sort-by (juxt (comp - :n) :industry))
       vec))

(defn- sic-table
  [rows]
  (let [groups (sic-summary rows)]
    [:div.table-wrap
     [:table
      [:thead
       [:tr
        [:th "industry"]
        [:th.num "n"]
        [:th.num "avg-mkt-cap ($M)"]]]
      (into [:tbody]
            (map (fn [{:keys [industry n avg-mkt-cap]}]
                   [:tr {:key industry}
                    [:td [:a (h/navigate :segment {} {:sic industry})
                          industry]]
                    [:td.num n]
                    [:td.num (if avg-mkt-cap
                               (format-value :millions avg-mkt-cap)
                               "")]]))
            groups)]]))

(defn reporting-cur
  "Reporting currency from `:revenue-unit`, or nil for USD / blank."
  [row]
  (when-let [u (some-> (:revenue-unit row) str str/trim not-empty)]
    (let [code (or (second (re-matches #"(?i)([A-Za-z]{3})(?:/.*)?" u)) u)]
      (when-not (= "USD" (str/upper-case code))
        (str/upper-case code)))))

(defn market-cap-adjusted
  "Reporting-currency market cap after ADR. Prefers `:market-cap-adjusted`
  on the row when present."
  [row]
  (or (when (number? (:market-cap-adjusted row))
        (double (:market-cap-adjusted row)))
      (stats/market-cap-adjusted row)))

(defn foreign-summary
  "One row per non-USD reporting currency. `:n` is the company count.
  `:avg-mkt-cap` is the mean USD `:marketcap` of companies that have one."
  [rows]
  (->> rows
       (filter reporting-cur)
       (group-by reporting-cur)
       (map (fn [[cur xs]]
              (let [mcaps (filterv number? (keep :marketcap xs))]
                {:reporting-cur cur
                 :n (count xs)
                 :avg-mkt-cap (when (seq mcaps)
                                (/ (reduce + (map double mcaps)) (count mcaps)))})))
       (sort-by (juxt (comp - :n) :reporting-cur))
       vec))

(defn- foreign-table
  [rows]
  (let [groups (foreign-summary rows)]
    [:div.table-wrap
     [:table
      [:thead
       [:tr
        [:th "reporting-cur"]
        [:th.num "n"]
        [:th.num "avg-mkt-cap ($M)"]]]
      (into [:tbody]
            (map (fn [{:keys [reporting-cur n avg-mkt-cap]}]
                   [:tr {:key reporting-cur}
                    [:td [:a (h/navigate :segment {} {:cur reporting-cur})
                          reporting-cur]]
                    [:td.num n]
                    [:td.num (if avg-mkt-cap
                               (format-value :millions avg-mkt-cap)
                               "")]]))
            groups)]]))

(defn- percentile-table
  [rows]
  (let [colspan (+ 2 percentile-count)]
    [:div.table-wrap
     [:table
      [:thead
       (into [:tr
              [:th "metric"]
              [:th.num "n"]]
             (map (fn [p]
                    [:th.num {:key p} (str "P" p)])
                  (range 1 (inc percentile-count))))]
      (into [:tbody]
            (mapcat (fn [{:keys [title metrics]}]
                      (cons [:tr.quintile-group {:key title}
                             [:th {:colspan colspan} title]]
                            (map #(metric-row rows %) metrics)))
                    groups))]]))

(defn overview-page
  [req]
  (let [loaded (try {:rows (load-rows)}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))]
    [:div.page
     (nav req)
     [:h1 "Stats"]
     (if-let [err (:error loaded)]
       [:p.error err]
       [:div
        [:p.quintile-note
         (format "%s companies. P1–P9 are the top of each decile; P10 is above P9."
                 (format "%,d" (count (:rows loaded))))]
        (percentile-table (:rows loaded))
        [:h2.stats-section "SIC"]
        (sic-table (:rows loaded))
        [:h2.stats-section "FOREIGN"]
        (foreign-table (:rows loaded))])]))
