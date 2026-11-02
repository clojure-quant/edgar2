(ns demo.xbrl-shares
  "Check `dei:EntityCommonStockSharesOutstanding` across multi-class filers.

  The reusable extraction and share-count heuristics live in
  `edgar2.filing.xbrl.xbrl-shares`. This demo selects the multi-class tickers,
  verifies `CommonStockTotal == sum(other classes)`, reports mismatches, and
  checkpoints results."
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [edgar2.download.dataset :as dataset]
   [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
   [edgar2.util.core :as ucore]))

(def shares-path
  "Input dataset produced by `edgar2.download.xbrl-shares`."
  "data/share-classes.edn")

(def checkpoint-path
  "Resumable results, one row per ticker."
  "data/xbrl-shares-multiclass-check.edn")

(def report-path
  "Markdown rendering of `checkpoint-path`."
  "data/xbrl-shares-multiclass-check.md")

(defn multi-class-rows
  "Rows of `shares-path` with more than one class, deduped by ticker."
  []
  (->> (dataset/read-rows shares-path)
       (filter #(> (count (:shares %)) 1))
       (reduce (fn [acc row]
                 (let [ticker (str/upper-case (str/trim (str (:ticker row))))]
                   (if (contains? acc ticker)
                     acc
                     (assoc acc ticker (assoc row :ticker ticker)))))
               {})
       vals
       (sort-by :ticker)
       vec))

(defn verify-shares
  "Primary identity for `shares`: total is the undimensioned fact, others are
  every class on a known class axis, and `UnknownClass` is reported apart."
  [shares]
  (let [total (get shares "CommonStockTotal")
        others (dissoc shares "CommonStockTotal" "UnknownClass")
        unknown (select-keys shares ["UnknownClass"])
        values (vals others)
        numeric? (every? number? values)
        sum (when (and numeric? (seq others)) (reduce + 0 values))
        diff (when (and (number? total) (number? sum)) (- total sum))
        status (cond
                 (empty? shares) :no-facts
                 (and (some? total) (empty? others)) :only-total
                 (nil? total) :no-total
                 (not numeric?) :invalid
                 (== total sum) :pass
                 :else :fail)]
    {:total total
     :others others
     :sum-others sum
     :unknown unknown
     :diff diff
     :status status}))

(defn- key-label [k]
  (if (keyword? k) (name k) (str k)))

(defn- common-or-ordinary? [k]
  (let [s (str/lower-case (key-label k))]
    (or (str/includes? s "common") (str/includes? s "ordinary"))))

(defn detect-aggregate
  "Secondary, advisory check: a dimensioned common/ordinary class that equals
  the sum of the other classes and is at least as large as each of them."
  [shares]
  (let [classes (dissoc shares "CommonStockTotal" "UnknownClass")
        shares* (into {} (filter (comp number? val)) classes)
        candidates (->> shares*
                        (filter (comp common-or-ordinary? key))
                        (sort-by (comp str key)))]
    (some (fn [[k v]]
            (let [others (vals (dissoc shares* k))
                  sum (reduce + 0 others)]
              (when (and (seq others)
                         (== v sum)
                         (every? #(<= % v) others))
                {:candidate k :value v :equals-others? true})))
          candidates)))

(defn- duplicate-classes [rows]
  (->> rows
       (map xbrl-shares/determine-class)
       frequencies
       (filter (fn [[_ n]] (> n 1)))
       (into {})))

(defn- error-message [^Throwable e]
  (if (instance? NullPointerException e)
    (str "no annual report document could be loaded"
         (when-let [m (.getMessage e)] (str ": " m)))
    (or (.getMessage e) (str (class e)))))

(defn- fetch-attempt
  "Fetch once, retrying one time after two seconds on any exception."
  [ticker]
  (loop [n 0]
    (let [result (try
                   {:rows (xbrl-shares/fetch-share-data ticker)}
                   (catch Throwable e
                     {:error e}))]
      (if (and (:error result) (< n 1))
        (do
          (Thread/sleep 2000)
          (recur (inc n)))
        result))))

(defn check-one [ticker]
  (let [{:keys [rows error]} (fetch-attempt ticker)]
    (if error
      {:ticker ticker
       :shares {}
       :status :error
       :total nil
       :others {}
       :sum-others nil
       :diff nil
       :unknown {}
       :duplicate-classes {}
       :aggregate nil
       :error (error-message error)}
      (let [shares (xbrl-shares/outstanding rows)]
        (merge (verify-shares shares)
               {:ticker ticker
                :shares shares
                :duplicate-classes (duplicate-classes rows)
                :aggregate (detect-aggregate shares)
                :error nil})))))

(defn- num-str [v]
  (cond
    (nil? v) ""
    (number? v) (let [d (double v)]
                  (if (and (== d (Math/rint d))
                           (<= (Math/abs d) (double Long/MAX_VALUE)))
                    (str (long d))
                    (format "%.4f" d)))
    :else (str v)))

(defn- pairs-str [m]
  (->> m
       (sort-by (comp str key))
       (map (fn [[k v]] (str (key-label k) "=" (num-str v))))
       (str/join "; ")))

(defn- duplicate-str [m]
  (->> m
       (sort-by (comp str key))
       (map (fn [[k n]] (str (key-label k) "=" n)))
       (str/join "; ")))

(defn- aggregate-str [aggregate]
  (when aggregate
    (str (key-label (:candidate aggregate)) "=" (num-str (:value aggregate)))))

(defn- row-cells [{:keys [ticker status total others sum-others diff aggregate
                          unknown duplicate-classes error]}]
  [ticker
   (name status)
   (num-str total)
   (pairs-str others)
   (num-str sum-others)
   (num-str diff)
   (or (aggregate-str aggregate) "")
   (if aggregate (if (:equals-others? aggregate) "yes" "no") "")
   (pairs-str unknown)
   (duplicate-str duplicate-classes)
   (or error "")])

(defn- md-cell [v]
  (-> (str (or v ""))
      (str/replace "|" "\\|")
      (str/replace #"\r?\n" " ")))

(defn- status-line [rows]
  (->> (frequencies (map :status rows))
       (sort-by key)
       (map (fn [[s n]] (str (name s) "=" n)))
       (str/join "  ")))

(defn- markdown [rows]
  (let [header ["Ticker" "Status" "CommonStockTotal" "Other classes"
                "Sum others" "Diff" "Aggregate candidate" "Aggregate = others"
                "Unclassified" "Duplicate classes" "Error"]
        separator (mapv (constantly "---") header)
        line (fn [row]
               (str "| " (str/join " | " (map md-cell (row-cells row))) " |"))]
    (str "# XBRL shares multi-class check\n\n"
         "Tickers: " (count rows) "  \n"
         "Status: " (status-line rows) "\n\n"
         "| " (str/join " | " header) " |\n"
         "| " (str/join " | " separator) " |\n"
         (str/join "\n" (map line rows))
         "\n")))

(defn- ordered-results [results rows]
  (->> rows
       (map :ticker)
       (keep results)
       vec))

(defn- checkpoint-meta []
  {:download-date (str (java.time.LocalDate/now))
   :source shares-path})

(defn- write-checkpoint! [results rows]
  (dataset/write-dataset! checkpoint-path (checkpoint-meta)
                          (ordered-results results rows)))

(defn- load-checkpoint []
  (->> (or (dataset/read-rows checkpoint-path) [])
       (keep (fn [row]
               (when (:ticker row)
                 [(str/upper-case (str (:ticker row))) row])))
       (into {})))

(defn- enable-cache! []
  (let [dir (io/file ucore/cache-dir)]
    (when-not (.isDirectory dir)
      (throw (ex-info (str "cache directory is not available: " ucore/cache-dir
                           " (is the external drive mounted?)")
                      {:cache-dir ucore/cache-dir})))
    (ucore/enable-cache)))

(defn- result-summary [results rows]
  (let [ordered (ordered-results results rows)
        mismatches (filterv #(contains? #{:fail :invalid} (:status %)) ordered)]
    (println)
    (println (format "checked %d multi-class tickers" (count ordered)))
    (doseq [[s n] (sort-by key (frequencies (map :status ordered)))]
      (println (format "  %-10s %d" (name s) n)))
    (println)
    (if (seq mismatches)
      (do
        (println (format "mismatches: %d (every :fail row has a non-zero :diff)"
                         (count mismatches)))
        (pprint/print-table
         [:ticker :status :total :sum-others :diff :unknown
          :duplicate-classes :error]
         mismatches))
      (println "mismatches: none with CommonStockTotal and other classes"))
    (println)
    (println (format "results  %s" checkpoint-path))
    (println (format "table    %s" report-path))
    ordered))

(defn run-check
  "Run the share check across the multi-class rows of `shares-path`.

  `:limit` processes the next n unchecked tickers for a smoke test.
  `:force` ignores the checkpoint. Without it, tickers with a non-error
  result are skipped, so an interrupted run resumes where it stopped."
  ([] (run-check {}))
  ([{:keys [limit force]}]
   (ucore/ensure-identity!)
   (enable-cache!)
   (let [rows (multi-class-rows)
         existing (if force {} (load-checkpoint))
         done? (fn [row] (and row (not= :error (:status row))))
         pending (->> rows
                      (remove #(done? (get existing (:ticker %))))
                      (take (or limit Long/MAX_VALUE))
                      vec)
         total (count pending)
         tickers (set (map :ticker rows))
         results (into {} (filter (fn [[ticker _]] (contains? tickers ticker)))
                       existing)]
     (println (format "xbrl-shares-multiclass-check  %d selected, %d to run (of %d multi-class)"
                      total total (count rows)))
     (flush)
     (let [results
           (loop [i 0
                  pending pending
                  results results]
             (if-let [row (first pending)]
               (let [ticker (:ticker row)
                     result (check-one ticker)
                     results' (assoc results ticker result)
                     i' (inc i)]
                 (when (or (zero? (mod i' 25)) (= i' total))
                   (println (format "  %d/%d  %s  %s"
                                    i' total ticker (name (:status result))))
                   (flush))
                 (when (zero? (mod i' 25))
                   (write-checkpoint! results' rows))
                 (when (< i' total)
                   (Thread/sleep 100))
                 (recur i' (next pending) results'))
               (do
                 (write-checkpoint! results rows)
                 (spit report-path (markdown (ordered-results results rows)))
                 (result-summary results rows)
                 results)))]
       results))))

(defn- parse-args [args]
  (loop [args (seq args)
         opts {:limit nil :force false}]
    (if-let [arg (first args)]
      (cond
        (= arg "--force")
        (recur (next args) (assoc opts :force true))

        (and (= arg "--limit") (second args))
        (recur (nnext args) (assoc opts :limit (parse-long (second args))))

        (str/starts-with? arg "--limit=")
        (recur (next args) (assoc opts :limit (parse-long (subs arg 8))))

        :else
        (recur (next args) opts))
      opts)))

(defn -main [& args]
  (run-check (parse-args args)))

(comment
  (def cpa (xbrl-shares/fetch-share-data "CPA"))
  cpa
  ;; ({:concept :EntityCommonStockSharesOutstanding, :value 41138553, :dimension {}}
  ;;  {:concept :EntityCommonStockSharesOutstanding, :value 30200428, :dimension #:ifrs-full{:ClassesOfShareCapitalAxis :cpa/ClassACommonStockMember}}
  ;;  {:concept :EntityCommonStockSharesOutstanding, :value 10938125, :dimension #:ifrs-full{:ClassesOfShareCapitalAxis :cpa/ClassBCommonStockMember}})

  (def f (xbrl-shares/fetch-share-data "F"))
  f
  ;({:concept :EntityCommonStockSharesOutstanding,  :value 3918623149,
  ;  :dimension #:us-gaap{:StatementClassOfStockAxis :us-gaap/CommonStockMember}  }
  ; {:concept :EntityCommonStockSharesOutstanding,
  ;  :value 70852076,
  ;  :dimension #:us-gaap{:StatementClassOfStockAxis :us-gaap/CommonClassBMember} })

  (xbrl-shares/outstanding f)
  (xbrl-shares/outstanding cpa)

 

  (multi-class-rows)
  (check-one "CPA")
  (run-check {:limit 10})
  (run-check {:limit 10 :force true})

  4567660012
  (+ 4504052678 63607334)

  ;; dei EntityCommonStockSharesOutstanding 2025-12-31 :us-gaap/CommonClass
  ;; :dei/AdrMember
  ;; dei EntityCommonStockSharesOutstanding 2025-12-31 :us-gaap/Com

  (xbrl-shares/check "NCTY")
  (xbrl-shares/check "TUYA")
  (xbrl-shares/check "BILI")
  ;; BILI
  {:us-gaap/CommonStockMember 414718112,
   :bili/CommonClassYMember    79700010,
   :bili/CommonClassZMember   335018102}
  (+ 335018102 79700010)

  ;
  )
