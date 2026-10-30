(ns demo.share-count-print
  "One ticker: each :shares-outstanding, :net-income, and :eps tag
  across the last 10 annual years, or the last 10 quarters when the
  second argument is Q.

  Rows are those concepts' :data tags, in priority order. A blank cell
  means that tag has no value for that period. Columns run oldest to newest.
  Shares and net income are millions; EPS is dollars per share."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.db.universe :as db-universe]
            [edgar2.download.facts :as dl]
            [edgar2.download.facts-universe :as universe]
            [edgar2.util :as util]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def n-periods
  "How many period columns to show."
  10)

(defn- concept-tags
  "`:data` tags for `id`, each tagged with how its cells are shown."
  [id scale]
  (mapv (fn [pair] {:pair pair :scale scale})
        (:data (concepts/get-concept-id id))))

(def table-tags
  "Rows of the history table, in print order."
  (vec (concat (concept-tags :shares-outstanding :millions)
               (concept-tags :net-income :millions)
               (concept-tags :eps :per-share))))

(defn- tag-name
  [[taxonomy concept]]
  (str (name taxonomy) ":" (name concept)))

(defn- universe-row
  [ticker]
  (let [want (str/upper-case (str/trim (str ticker)))]
    (some #(when (= want (str/upper-case (str (:ticker %)))) %)
          (db-universe/load-universe))))

(defn- read-cik-json
  [zf cik]
  (when-let [entry (.getEntry zf (str "CIK" (universe/pad-cik cik) ".json"))]
    (with-open [in (.getInputStream zf entry)]
      (json/read-value in json/keyword-keys-object-mapper))))

(defn- millions
  "Amount in millions. Nil stays blank. A positive amount under
  half a million is shown with two decimals so it does not look missing."
  [v]
  (when (number? v)
    (let [m (/ (double v) 1.0e6)]
      (if (>= (Math/abs m) 0.5)
        (long (Math/round m))
        (format "%.2f" m)))))

(defn- year-labels
  "Column headings for annual `periods`, oldest first. A repeated calendar
  year keeps the full period-end."
  [periods]
  (let [years (mapv #(subs (str (:end %)) 0 4) periods)]
    (if (apply distinct? years)
      years
      (mapv #(str (:end %)) periods))))

(defn- period-labels
  "Column headings, oldest first. Annual columns are calendar years.
  Quarterly columns are `fy-fp` (2025-Q2), or the period-end when those
  labels collide."
  [q-or-y periods]
  (cond
    (empty? periods) []
    (= q-or-y :q)
    (let [labels (mapv #(str (:period %)) periods)]
      (if (and (every? seq labels)
               (apply distinct? labels))
        labels
        (mapv #(str (:end %)) periods)))
    :else (year-labels periods)))

(defn- per-share
  "EPS as reported. Nil stays blank. Trailing zeros are dropped."
  [v]
  (when (number? v)
    (let [d (double v)]
      (if (and (== d (Math/rint d))
               (<= (Math/abs d) (double Long/MAX_VALUE)))
        (long d)
        (-> (format "%.4f" d)
            (str/replace #"0+$" "")
            (str/replace #"\.$" ""))))))

(defn- cell
  [scale v]
  (case scale
    :millions (millions v)
    :per-share (per-share v)))

(defn- values-by-tag
  "period-end → fact, per `[taxonomy concept]`. A later `:filed` wins."
  [long-ds]
  (reduce (fn [acc row]
            (let [k [(str (:taxonomy row)) (str (:concept row))]
                  end (str (:end row))
                  cur (get-in acc [k end])]
              (if (or (nil? cur)
                      (pos? (compare (str (:filed row)) (str (:filed cur)))))
                (assoc-in acc [k end] row)
                acc)))
          {}
          (ds/rows long-ds {:nil-missing? true})))

(defn- history-rows
  "One map per tag. Keys are `:tag` plus each year label.

  A cell is that tag's own taxonomy, so `us-gaap:ProfitLoss` and
  `ifrs-full:ProfitLoss` stay separate."
  [by-tag periods labels]
  (mapv (fn [{:keys [pair scale]}]
          (let [[taxonomy concept] pair
                by-end (get by-tag [(name taxonomy) (name concept)])]
            (into {:tag (tag-name pair)}
                  (map (fn [label period]
                         [label (cell scale (:val (get by-end (str (:end period)))))])
                       labels
                       periods))))
        table-tags))

(defn- frequency
  "`:q` when `arg` is Q, otherwise annual."
  [arg]
  (if (= "Q" (some-> arg str/trim str/upper-case))
    :q
    :y))

(defn run
  "Print the last 10 periods of every `:shares-outstanding`, `:net-income`,
  and `:eps` tag for `ticker`.

  `q-or-y` is `:q` for quarters and `:y` for years. Shares and net income
  are millions. EPS is dollars per share. The header line is the share count
  `shares-outstanding` selects, with its period and period-end."
  ([ticker] (run ticker :y))
  ([ticker q-or-y]
   (util/ensure-identity!)
   (let [ticker (str/upper-case (str/trim (str ticker)))
         quarterly? (= q-or-y :q)
         row (universe-row ticker)
         cik (or (:cik row) (util/resolve-filer2 ticker))
         zip-path (dl/ensure-zip! {})]
     (with-open [zf (ZipFile. (str zip-path))]
       (if-let [data (read-cik-json zf cik)]
         (let [dss (xbrl/facts->dataset data)
               obs (universe/shares-outstanding-obs dss)
               long-ds (try
                         (facts-ds/select-data dss q-or-y (map :pair table-tags))
                         (catch Throwable ex
                           (println (format "%s  %s  CIK=%s" ticker (:entityName data) cik))
                           (println (str (if quarterly? "no quarterly periods: " "no annual periods: ")
                                         (.getMessage ex)))
                           nil))
               by-tag (when long-ds (values-by-tag long-ds))
               periods (when long-ds
                         (->> (ds/rows long-ds {:nil-missing? true})
                              (group-by #(str (:end %)))
                              (sort-by key #(compare %2 %1))
                              (take n-periods)
                              (map (fn [[_ rows]] (first rows)))
                              reverse
                              vec))
               labels (period-labels q-or-y periods)
               span (if quarterly? "quarter" "year")]
           (println (format "%s  %s  CIK=%s"
                            ticker (or (:entityName data) (:entityName row)) cik))
           (println (format "shares-outstanding  %s million  %s  %s"
                            (or (millions (:val obs)) "—")
                            (or (:period obs) "—")
                            (or (:end obs) "—")))
           (println (format "shares and net income in millions; EPS in dollars per share; a blank cell means that tag is not reported for the %s" span))
           (when (seq periods)
             (pprint/print-table
              (into [:tag] labels)
              (history-rows by-tag periods labels)))
           nil)
         (println (format "no companyfacts for %s (CIK %s)" ticker cik)))))))

(defn -main
  [& args]
  (let [ticker (some-> (first args) str/trim not-empty)
        freq (some-> (second args) str/trim not-empty)]
    (cond
      (nil? ticker) (println "Usage: clj -M -m demo.share-count-print TICKER [Q]")
      (and freq (not= "Q" (str/upper-case freq)))
      (println "Usage: clj -M -m demo.share-count-print TICKER [Q]")
      :else (run ticker (frequency freq)))))
