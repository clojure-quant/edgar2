(ns demo.sharecount-check
  "Compare the facts-universe share count with shares implied by earnings.

  The share count is `edgar2.download.facts-universe/shares-outstanding`.
  The estimate is |:net-income| / |:eps| from that share count's own period
  (concept priority in `edgar2.financials.concepts`). A 10-Q share count is
  compared with that quarter; an annual share count with that year.
  `edgar2.financials.calc.annual/ratio` only divides a pair of values from
  the same report-table row, so a quarterly share count is never compared
  with annual earnings. Prints filers where the two figures differ by more
  than 50%."
  (:require [clojure.pprint :as pprint]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.download.facts :as dl]
            [edgar2.download.facts-universe :as universe]
            [edgar2.db.universe :as db-universe]
            [edgar2.financials.calc.annual :as annual]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def earnings-tags
  (:data (concepts/get-concept-id :net-income)))

(def eps-tags
  (:data (concepts/get-concept-id :eps)))

(defn read-cik-json
  [zf cik]
  (when cik
    (when-let [entry (.getEntry zf (str "CIK" cik ".json"))]
      (with-open [in (.getInputStream zf entry)]
        (json/read-value in json/keyword-keys-object-mapper)))))

(defn- frequency
  "`:q` for a 10-Q share count, `:y` for an annual report. Nil otherwise."
  [form]
  (cond
    (universe/quarterly-share-forms (str form)) :q
    (universe/annual-share-forms (str form)) :y))

(defn- share-period
  "Report-table period of the share count, e.g. \"2011-Q1\" or \"2010-FY\"."
  [{:keys [fy fp]}]
  (when (and fy (not-empty (str fp)))
    (str fy "-" fp)))

(defn- periods
  [dss q-or-y]
  (case q-or-y
    :y (facts-ds/annual-periods dss)
    :q (facts-ds/quarterly-periods dss)))

(defn- form-by-end
  "Period-end → form for `q-or-y` periods in `dss`."
  [dss q-or-y]
  (try
    (into {}
          (keep (fn [row]
                  (when-let [form (not-empty (str (:form row)))]
                    [(str (:end row)) form])))
          (ds/rows (periods dss q-or-y) {:nil-missing? true}))
    (catch Throwable _
      {})))

(defn- abs-num
  [v]
  (when (number? v)
    (Math/abs (double v))))

(defn estimate-shares
  "|earnings| / |EPS| for one period, from the :net-income and :eps concepts.

  `q-or-y` is the share count's frequency. `period` is its `fy-fp`
  (\"2011-Q1\", \"2010-FY\"). Both figures are taken from that row.
  `annual/ratio` divides newest-first vectors that stay aligned with the
  report table, so this never pairs a quarter's shares with a year's
  earnings. Nil when that period lacks either figure, or EPS is zero."
  [dss q-or-y period]
  (when (and q-or-y period)
    (let [table (facts-ds/report-table dss q-or-y (concat earnings-tags eps-tags))
          rows (vec (ds/rows table {:nil-missing? true}))
          earnings (facts-ds/calc-concept table :net-income)
          eps (facts-ds/calc-concept table :eps)
          implied (annual/ratio (mapv abs-num earnings) (mapv abs-num eps))
          forms (form-by-end dss q-or-y)]
      (some (fn [[row earn e est]]
              (when (and (= period (str (:period row)))
                         (number? est)
                         (pos? (double est)))
                {:end (str (:end row))
                 :period period
                 :form (get forms (str (:end row)))
                 :earnings (double earn)
                 :eps (double e)
                 :estimated (double est)}))
            (map vector rows earnings eps implied)))))

(defn off-fraction
  "Distance of `shares` from `estimated`, as a fraction of the estimate.
  0.5 means 50%."
  [shares estimated]
  (Math/abs (- (/ (double shares) (double estimated)) 1.0)))

(defn off-by-half?
  "True when `shares` differs from `estimated` by more than 50%."
  [shares estimated]
  (and (number? shares)
       (pos? (double shares))
       (number? estimated)
       (pos? (double estimated))
       (> (off-fraction shares estimated) 0.5)))

(defn- check-company
  "Share count against the earnings estimate.

  Returns a map when both figures exist. `:off?` is true when they differ
  by more than 50%. Nil when companyfacts is missing or either figure is."
  [data]
  (when data
    (let [obs (universe/shares-outstanding-obs (:facts data))
          shares (:val obs)
          q-or-y (frequency (:form obs))]
      (when (and (number? shares) (pos? (double shares)) q-or-y)
        (when-let [est (try
                         (estimate-shares (xbrl/facts->dataset data)
                                          q-or-y
                                          (share-period obs))
                         (catch Throwable _ nil))]
          (let [estimated (:estimated est)]
            {:shares (double shares)
             :share-form (not-empty (str (:form obs)))
             :earn-form (:form est)
             :end (:end est)
             :period (:period est)
             :earnings (:earnings est)
             :eps (:eps est)
             :estimated estimated
             :off (off-fraction shares estimated)
             :off? (off-by-half? shares estimated)}))))))

(defn- millions
  [x]
  (long (Math/round (/ (double x) 1.0e6))))

(defn- amount
  "Decimal without scientific notation. Whole amounts stay whole."
  [x]
  (when (number? x)
    (let [d (double x)]
      (if (and (== d (Math/rint d))
               (<= (Math/abs d) (double Long/MAX_VALUE)))
        (str (long d))
        (format "%.4f" d)))))

(defn- row-out
  [{:keys [ticker entityName share-form period shares estimated earnings eps off]}]
  {:ticker ticker
   :name (let [s (str entityName)]
           (if (> (count s) 32) (subs s 0 32) s))
   :share-form share-form
   :period period
   :shares-m (millions shares)
   :estimated-m (millions estimated)
   :earnings (amount earnings)
   :eps (amount eps)
   :off (format "%.0f%%" (* 100.0 off))})

(defn run
  "Scan the universe. Print filers whose share count is off by more than 50%
  from |net income| / |EPS| of that same quarter or year.

  `:limit` n checks only the first n universe rows."
  ([] (run {}))
  ([{:keys [limit]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {})
         filers (vec (cond->> (db-universe/load-universe)
                       limit (take (long limit))))
         total (count filers)]
     (println (format "sharecount-check  %d universe  ← %s" total zip-path))
     (flush)
     (with-open [zf (ZipFile. (str zip-path))]
       (loop [rows filers
              i 0
              compared 0
              off []]
         (if-let [row (first rows)]
           (let [data (try
                        (read-cik-json zf (:cik row))
                        (catch Throwable _
                          nil))
                 hit (when data
                       (try
                         (check-company data)
                         (catch Throwable _
                           nil)))
                 compared' (if hit (inc compared) compared)
                 off' (if (:off? hit)
                        (conj off (merge hit (select-keys row [:ticker :entityName])))
                        off)
                 i' (inc i)]
             (when (or (zero? (mod i' 250)) (= i' total))
               (println (format "  %d/%d  compared %d  off %d"
                                i' total compared' (count off')))
               (flush))
             (recur (next rows) i' compared' off'))
           (let [shown (->> off
                            (sort-by :off >)
                            (mapv row-out))]
             (println)
             (println (format "off by more than 50%%: %d of %d with both figures (scanned %d)"
                              (count shown) compared total))
             (println "share-form is the filing behind the share count. shares-m and estimated-m are millions. earnings is the reported amount; eps is per share. estimated shares are |earnings| / |eps|.")
             (pprint/print-table
              [:ticker :name :share-form :period :shares-m :estimated-m :earnings :eps :off]
              shown)
             shown)))))))

(defn -main
  [& args]
  (run (when-let [n (first args)]
         {:limit (parse-long n)})))
