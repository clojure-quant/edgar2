(ns edgar2.institutional
  "13F holdings from the local EDGAR index and daily feeds.

  edgar2.filing.quarterly-index reads each master.zip for one year. Those
  rows are reduced to data/13f-index-<year>.edn, the list of 13F-HR and
  13F-HR/A filings to process.

  Each filing is then read from the daily archive via edgar2.filing.day.
  edgar2.filing.complete reads that submission once. A day that is not
  downloaded is skipped. Stage 1 writes
  data/holdings-raw-<year>.nippy, one submission per record. Share lines
  (sshPrnamtType SH, no put or call) keep their column-7 manager numbers.
  Lines that share a CUSIP, discretion, and those numbers are summed.
  :total-value is the sum of those values, in dollars (filings before
  2023-01-03 reported thousands).

  Stage 2 reads the raw file, drops a column-7 share count when the named
  manager's own filing for that quarter lists the same CUSIP, and writes
  data/holdings-<year>.nippy.

  Usage:
    (print-filers {:year 2026})
    clj -X:institutional :year 2026"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [edgar2.db.holdings :as holdings]
            [edgar2.filing.complete :as complete]
            [edgar2.filing.day :as day]
            [edgar2.filing.quarterly-index :as qi])
  (:import [java.io ByteArrayInputStream]
           [java.time LocalTime]
           [java.time.format DateTimeFormatter]))

(defn- require-year
  [year]
  (if (pos-int? year)
    (long year)
    (throw (ex-info "year is required" {:year year}))))

(defn index-path
  "13F filing index for `year`, data/13f-index-<year>.edn."
  [year]
  (str "data/13f-index-" year ".edn"))

(def holdings-forms #{"13F-HR" })

(def ^:private progress-ms 30000)

(def ^:private clock
  (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn- gb
  [bytes]
  (let [g (/ (double bytes) 1073741824.0)
        s (if (>= g 10.0) (format "%.0f" g) (format "%.1f" g))]
    (str (str/replace s #"\.0$" "") " gb")))

(defn- print-progress
  [{:keys [done total files]}]
  (let [names (sort (keys files))
        width (reduce max 1 (map count names))]
    (println (format "%s  %d/%d filings processed"
                     (.format clock (LocalTime/now))
                     (long done)
                     (long total)))
    (doseq [name names
            :let [{:keys [done total bytes]} (get files name)]]
      (println (format "%s  %d/%d (%s)"
                       (format (str "%-" width "s") name)
                       (long done)
                       (long total)
                       (gb bytes))))
    (flush)))

(defn- start-progress!
  "Print the in-flight archives every 30 seconds until `stop` is set."
  [progress stop]
  (doto (Thread.
          (fn []
            (loop [next (System/currentTimeMillis)]
              (when-not (realized? stop)
                (let [now (System/currentTimeMillis)]
                  (if (>= now next)
                    (do (print-progress @progress)
                        (recur (+ now progress-ms)))
                    (do (Thread/sleep 250)
                        (recur next))))))))
    (.setName "13f-progress")
    (.setDaemon true)
    .start))

(defn- track-start!
  [progress name total bytes]
  (swap! progress update :files assoc name
         {:done 0 :total total :bytes bytes}))

(defn- track-filing!
  [progress name]
  (swap! progress
         (fn [p]
           (-> p
               (update :done inc)
               (update-in [:files name :done] (fnil inc 0))))))

(defn- track-finish!
  [progress name]
  (swap! progress update :files dissoc name))

(defn- report-period
  "Cover `periodOfReport` is MM-DD-YYYY. Store YYYY-MM-DD."
  [s]
  (cond
    (re-matches #"\d{4}-\d{2}-\d{2}" s) s
    (re-matches #"\d{2}-\d{2}-\d{4}" s)
    (let [[m d y] (str/split s #"-")]
      (str y "-" m "-" d))
    :else s))

(defn- tag-name
  [node-tag tag]
  (or (= tag node-tag)
      (when (keyword? node-tag)
        (= (name tag) (last (str/split (name node-tag) #":"))))))

(defn- find-tag
  [node tag]
  (when (map? node)
    (if (tag-name (:tag node) tag)
      node
      (some #(find-tag % tag) (:content node)))))

(defn- find-tags
  [node tag]
  (when (map? node)
    (let [here (if (tag-name (:tag node) tag) [node] [])]
      (into here (mapcat #(find-tags % tag) (:content node))))))

(defn- tag-text
  [node tag]
  (some-> (find-tag node tag) :content first str str/trim not-empty))

(defn- child-text
  [node tag]
  (some->> (:content node)
           (filter #(and (map? %) (tag-name (:tag %) tag)))
           first
           :content
           first
           str
           str/trim
           not-empty))

(defn- parse-long-safe
  [s]
  (when (not-empty (str/trim (or s "")))
    (try (parse-long (str/replace s "," ""))
         (catch Exception _ nil))))

(defn- parse-xml
  [s]
  (let [xml (cond
              (str/includes? s "<XML>")
              (subs s
                    (+ (str/index-of s "<XML>") 5)
                    (or (str/last-index-of s "</XML>") (count s)))
              :else s)
        xml (str/trim xml)]
    (when (str/starts-with? xml "<")
      (xml/parse (ByteArrayInputStream. (.getBytes xml "UTF-8"))))))

(defn- other-managers
  "Column 7 sequence numbers. Blank and N/A are none. Stored in ascending order."
  [s]
  (let [t (some-> s str/trim)]
    (if (or (str/blank? t) (= "N/A" (str/upper-case t)))
      []
      (->> (str/split t #",")
           (keep #(parse-long-safe (str/trim %)))
           distinct
           sort
           vec))))

(defn- manager-ref
  [node]
  {:cik (parse-long-safe (tag-text node :cik))
   :name (tag-text node :name)
   :file-number (tag-text node :form13FFileNumber)})

(defn- included-managers
  "Summary-page list. Sequence number to the manager this filing reports for."
  [cover]
  (into {}
        (keep (fn [node]
                (when-let [n (parse-long-safe (tag-text node :sequenceNumber))]
                  (let [m (or (find-tag node :otherManager) node)]
                    [n (manager-ref m)]))))
        (find-tags cover :otherManager2)))

(defn- reported-by
  "Cover list of managers who report the slice omitted from this table."
  [cover]
  (if-let [info (find-tag cover :otherManagersInfo)]
    (mapv manager-ref (find-tags info :otherManager))
    []))

(defn- holding-rows
  [root]
  (if-not root
    []
    (mapv (fn [entry]
            (let [amt (find-tag entry :shrsOrPrnAmt)]
              {:cusip (tag-text entry :cusip)
               :issuer (tag-text entry :nameOfIssuer)
               :class (tag-text entry :titleOfClass)
               :value (parse-long-safe (tag-text entry :value))
               :shares (parse-long-safe (child-text amt :sshPrnamt))
               :shares-type (child-text amt :sshPrnamtType)
               :put-call (tag-text entry :putCall)
               :discretion (some-> (tag-text entry :investmentDiscretion) str/upper-case)
               :other-managers (other-managers (tag-text entry :otherManager))}))
          (find-tags root :infoTable))))

(defn- usd
  [n filing-date]
  (cond
    (nil? n) nil
    (and filing-date (neg? (compare filing-date "2023-01-03"))) (* n 1000)
    :else n))

(defn- stock-holdings
  "Share lines, summed when CUSIP, discretion, and column 7 match.
  A sole line and a column-7 line of the same CUSIP stay separate."
  [rows filing-date]
  (->> rows
       (filter (fn [{:keys [put-call shares-type shares cusip]}]
                 (and (not-empty cusip)
                      (str/blank? (str put-call))
                      (= "SH" (some-> shares-type str/upper-case))
                      (and shares (pos? shares)))))
       (map (fn [row]
              {:cusip (:cusip row)
               :issuer (:issuer row)
               :class (:class row)
               :qty (:shares row)
               :value (usd (:value row) filing-date)
               :discretion (:discretion row)
               :other-managers (:other-managers row)}))
       (group-by (juxt :cusip :discretion :other-managers))
       (map (fn [[_ group]]
              (assoc (first group)
                     :qty (reduce + 0 (keep :qty group))
                     :value (reduce + 0 (keep :value group)))))
       (sort-by (juxt :cusip :discretion :other-managers))
       vec))

(defn get-holdings
  "Essence of one 13F submission string.

  `filing` is {:submission \"<SUBMISSION>...\"}. Returns nil when the text
  has no information table.

  {:filer 1067983
   :filer-name \"BERKSHIRE HATHAWAY INC\"
   :filing-date \"2026-08-14\"
   :report-period \"2025-12-31\"
   :report-type \"13F HOLDINGS REPORT\"
   :amendment-type nil
   :amendment-no nil
   :file-number \"028-12345\"
   :included-managers {}
   :reported-by []
   :total-count 1
   :total-value 25000000
   :holdings [{:cusip \"037833100\"
               :issuer \"APPLE INC\"
               :class \"COM\"
               :qty 123455
               :value 25000000
               :discretion \"SOLE\"
               :other-managers []}]}"
  [{:keys [submission]}]
  (when submission
    (let [structure (complete/file-structure submission)
          {:keys [cik name filing-date form]} (:header structure)
          info (complete/load-file structure ".xml" "INFORMATION TABLE")
          cover-xml (complete/load-file structure ".xml" (or form "13F-HR"))
          rows (holding-rows (when info (parse-xml info)))
          cover (when cover-xml (parse-xml cover-xml))
          page (when cover (find-tag cover :coverPage))
          holdings (stock-holdings rows filing-date)]
      (when (seq rows)
        {:filer (or cik 0)
         :filer-name (or (when cover (tag-text (find-tag cover :filingManager) :name))
                         name)
         :filing-date filing-date
         :report-period (some-> cover (tag-text :periodOfReport) report-period)
         :report-type (when page (child-text page :reportType))
         :amendment-type (some-> cover (find-tag :amendmentInfo) (child-text :amendmentType))
         :amendment-no (some-> page (child-text :amendmentNo) parse-long-safe)
         :file-number (when page (child-text page :form13FFileNumber))
         :included-managers (if cover (included-managers cover) {})
         :reported-by (if cover (reported-by cover) [])
         :total-count (count holdings)
         :total-value (reduce + 0 (keep :value holdings))
         :holdings holdings}))))

(defn- write-edn!
  [path data]
  (let [file (io/file path)
        tmp (str path ".tmp")]
    (some-> (.getParentFile file) .mkdirs)
    (spit tmp (binding [*print-length* nil
                        *print-level* nil]
                (with-out-str (pprint/pprint data))))
    (.renameTo (io/file tmp) file)
    path))

(defn- accession
  [filename]
  (second (re-find #"(\d{10}-\d{2}-\d{6})" (str filename))))

(defn- index-row
  [row]
  (when-let [acc (accession (:filename row))]
    {:cik (:cik row)
     :name (:company-name row)
     :form (:form row)
     :filing-date (:date-filed row)
     :accession acc}))

(defn write-index!
  "Write data/13f-index-<year>.edn from the quarterly master.zip files for `year`.

  {:filings [{:cik :name :form :filing-date :accession} ...]}"
  [year]
  (let [year (require-year year)
        quarters (qi/on-disk {:year year})
        path (index-path year)]
    (when (empty? quarters)
      (throw (ex-info (str "No quarterly master.zip for " year " under " qi/*index-dir*)
                      {:dir qi/*index-dir* :year year})))
    (let [filings (->> quarters
                       (mapcat (fn [quarter]
                                 (println (format "  index %d QTR%d"
                                                  (:year quarter)
                                                  (:quarter quarter)))
                                 (flush)
                                 (mapcat #(qi/query-form quarter %) holdings-forms)))
                       (keep index-row)
                       (group-by :accession)
                       (map (comp first val))
                       (sort-by (juxt :filing-date :accession))
                       vec)
          data {:filings filings}]
      (write-edn! path data)
      (println (format "Wrote %s  (%d filings)" path (count filings)))
      (flush)
      data)))

(defn- load-day
  [progress archive rows]
  (let [name (.getName archive)]
    (track-start! progress name (count rows) (.length archive))
    (try
      (let [bodies (day/read-accessions archive (map :accession rows))]
        (into []
              (remove nil?)
              (mapv (fn [row]
                      (let [result
                            (if-let [text (get bodies (:accession row))]
                              (try
                                (some-> (get-holdings {:submission text})
                                        (assoc :accession (:accession row)
                                               :form (:form row)))
                                (catch Exception ex
                                  (println (format "    skip %s %s" (:accession row) (.getMessage ex)))
                                  nil))
                              (do (println (format "    missing %s" (:accession row)))
                                  nil))]
                        (track-filing! progress name)
                        result))
                    rows)))
      (finally
        (track-finish! progress name)))))

(defn load-holdings
  "Holdings for index filings whose daily archive is already on disk."
  [filings]
  (let [by-day (group-by :filing-date filings)
        days (sort (keys by-day))
        present (filter day/archive-file days)
        skipped (remove day/archive-file days)]
    (println (format "%d filings in the index, %d days on disk, %d days skipped"
                     (count filings)
                     (count present)
                     (count skipped)))
    (flush)
    (let [total (reduce + 0 (map #(count (get by-day %)) present))
          progress (atom {:done 0 :total total :files {}})
          stop (promise)
          _ (start-progress! progress stop)]
      (try
        (->> present
             (pmap (fn [day]
                     (load-day progress (day/archive-file day) (get by-day day))))
             (mapcat identity)
             ;; (sort-by (juxt :filing-date :filer))
             vec)
        (finally
          (deliver stop true)
          (print-progress @progress))))))

(defn- filing-order
  [filing]
  [(:filing-date filing) (or (:amendment-no filing) 0)])

(defn- owner-of
  "Lowest column-7 sequence, resolved to a CIK or else a 13F file number.
  Nil when the line names no other manager."
  [filing row]
  (when-let [seqs (seq (:other-managers row))]
    (let [m (get (:included-managers filing) (apply min seqs))]
      (cond
        (:cik m) [:cik (:cik m)]
        (not-empty (:file-number m)) [:file (:file-number m)]))))

(defn- annotate-owners
  [filing]
  (update filing :holdings
          (fn [rows]
            (mapv #(assoc % :owner (owner-of filing %)) (or rows [])))))

(defn- collapse-filer
  "One book for a CIK and quarter. A restatement replaces. NEW HOLDINGS adds."
  [filings]
  (reduce (fn [book filing]
            (cond
              (nil? book) filing
              (= "NEW HOLDINGS" (:amendment-type filing))
              (-> book
                  (update :holdings into (:holdings filing))
                  (assoc :filing-date
                         (if (pos? (compare (:filing-date filing) (:filing-date book)))
                           (:filing-date filing)
                           (:filing-date book))))
              :else filing))
          nil
          (sort-by filing-order filings)))

(defn- several-filers?
  [filing]
  (or (seq (:included-managers filing))
      (some #(seq (:other-managers %)) (:holdings filing))))

(defn- book-qty
  [book cusip]
  (reduce + 0
          (keep (fn [h]
                  (when (= cusip (:cusip h)) (:qty h)))
                (:holdings book))))

(defn- value-removed
  [value qty removed]
  (cond
    (or (nil? value) (not (pos? qty)) (not (pos? removed))) 0
    (>= removed qty) value
    :else (long (Math/round (* (double value) (/ (double removed) (double qty)))))))

(defn- take-qty
  "Remove `n` shares from `rows`, largest row first. Returns removal pairs."
  [n rows]
  (loop [left n
         rows (sort-by :qty #(compare %2 %1) rows)
         acc []]
    (if (or (not (pos? left)) (empty? rows))
      acc
      (let [row (first rows)
            take (min left (:qty row))]
        (recur (- left take)
               (rest rows)
               (if (pos? take)
                 (conj acc [[(:bi row) (:ri row)] take])
                 acc))))))

(defn- claim-removals
  "Qty to drop from each [book-index row-index].

  Rows on one filing for the same included manager and CUSIP are one claim.
  The largest claim is kept. Smaller filings that name the same manager drop
  those rows. The kept claim then drops only the shares that manager's own
  filing also lists."
  [books]
  (let [by-key (reduce (fn [idx book]
                         (cond-> idx
                           (:filer book) (assoc [:cik (:filer book)] book)
                           (not-empty (:file-number book))
                           (assoc [:file (:file-number book)] book)))
                       {}
                       books)
        claims (vec (for [[bi book] (map-indexed vector books)
                          [ri row] (map-indexed vector (:holdings book))
                          :when (:owner row)]
                      {:bi bi
                       :ri ri
                       :owner (:owner row)
                       :cusip (:cusip row)
                       :qty (or (:qty row) 0)
                       :filer (:filer book)}))]
    (reduce (fn [acc [_owner-cusip pools]]
              (let [by-filer (group-by :filer pools)
                    ranked (sort-by (fn [[filer rows]]
                                      [(- (reduce + 0 (map :qty rows))) filer])
                                    by-filer)
                    [win-filer win-rows] (first ranked)
                    losers (mapcat val (rest ranked))
                    win-qty (reduce + 0 (map :qty win-rows))
                    b (get by-key (:owner (first win-rows)))
                    overlap (if (or (nil? b)
                                    (= win-filer (:filer b))
                                    (zero? (book-qty b (:cusip (first win-rows)))))
                             0
                             (min win-qty (book-qty b (:cusip (first win-rows)))))]
                (into acc
                      (concat (map (fn [row] [[(:bi row) (:ri row)] (:qty row)]) losers)
                              (take-qty overlap win-rows)))))
            {}
            (group-by (juxt :owner :cusip) claims))))

(defn- apply-removals
  "Returns [book cuts]. `cuts` are {:cusip :gone} for every row that lost shares."
  [book removals bi]
  (let [result (reduce (fn [acc [ri row]]
                         (let [removed (get removals [bi ri])
                               qty (or (:qty row) 0)]
                           (if-not (and removed (pos? removed))
                             (update acc :rows conj (dissoc row :owner))
                             (let [gone (value-removed (:value row) qty removed)]
                               (-> acc
                                   (update :cuts conj {:cusip (:cusip row) :gone gone})
                                   (update :rows
                                           (fn [rows]
                                             (if (>= removed qty)
                                               rows
                                               (conj rows (assoc (dissoc row :owner)
                                                                 :qty (- qty removed)
                                                                 :value (- (or (:value row) 0) gone)))))))))))
                       {:rows [] :cuts []}
                       (map-indexed vector (:holdings book)))]
    [(assoc book :holdings (vec (:rows result)))
     (:cuts result)]))

(defn- finish-book
  "One row per CUSIP, without the column-7 fields the demo does not read."
  [book]
  (let [holdings (->> (:holdings book)
                      (filter #(and (:qty %) (pos? (:qty %))))
                      (group-by :cusip)
                      (map (fn [[cusip rows]]
                             {:cusip cusip
                              :issuer (some :issuer rows)
                              :class (some :class rows)
                              :qty (reduce + 0 (keep :qty rows))
                              :value (reduce + 0 (keep :value rows))}))
                      (sort-by :cusip)
                      vec)]
    {:filer (:filer book)
     :filer-name (:filer-name book)
     :filing-date (:filing-date book)
     :report-period (:report-period book)
     :total-count (count holdings)
     :total-value (reduce + 0 (keep :value holdings))
     :holdings holdings}))

(defn prune-holdings
  "Drop duplicate column-7 shares from stage-1 `filings`.

  Returns {:filers :several-filers :cusips-reduced :value-removed}.
  :several-filers counts filings that name another manager.
  :cusips-reduced counts distinct CUSIPs whose shares were removed.
  :value-removed is the dollar value of those removed shares."
  [filings]
  (let [several (count (filter several-filers? filings))
        annotated (map annotate-owners filings)
        periods (group-by :report-period annotated)
        pruned (mapcat (fn [[_period books]]
                         (let [collapsed (->> books
                                              (group-by :filer)
                                              vals
                                              (map collapse-filer)
                                              vec)
                               removals (claim-removals collapsed)]
                           (map-indexed (fn [bi book]
                                          (let [[book cuts] (apply-removals book removals bi)]
                                            {:book (finish-book book)
                                             :cuts cuts}))
                                        collapsed)))
                       periods)
        cuts (mapcat :cuts pruned)
        filers (->> pruned
                    (map :book)
                    (sort-by (juxt :report-period :filer))
                    vec)]
    {:filers filers
     :several-filers several
     :cusips-reduced (count (distinct (map :cusip cuts)))
     :value-removed (reduce + 0 (keep :gone cuts))}))

(defn write-holdings!
  [year filers]
  (let [year (require-year year)
        path (holdings/raw-path year)]
    (println (format "writing %s .." path))
    (flush)
    (holdings/write-raw year filers)
    (println (format "writing %s .. done!" path))
    (println (format "Wrote %s  (%d filings)" path (count filers)))
    (flush)
    filers))

(defn- money
  [n]
  (if n (format "$%,d" (long n)) ""))

(defn- print-table
  [filers]
  (let [rows (->> filers
                  (map (fn [h]
                         {:filer (:filer h)
                          :name (:filer-name h)
                          :holdings (count (:holdings h))
                          :value (:total-value h)}))
                  (sort-by #(or (:value %) -1) #(compare %2 %1))
                  vec)
        total (reduce + 0 (keep :value rows))]
    (println (format "%d filers   %s" (count rows) (money total)))
    (pprint/print-table
     [:filer :name :holdings :value]
     (map #(update % :value money) rows))
    nil))

(defn- report-prune
  [{:keys [several-filers cusips-reduced value-removed]}]
  (println (format "%d filings have more than one filer" several-filers))
  (println (format "%d cusips reduced" cusips-reduced))
  (println (format "%s value removed" (money value-removed)))
  (flush))

(defn prune-filers
  "Read data/holdings-raw-<year>.nippy, drop duplicate column-7 shares, and
  write data/holdings-<year>.nippy. Prints how many filings name another
  manager, how many CUSIPs lost shares, and the value of the shares removed."
  [year]
  (let [year (require-year year)
        result (prune-holdings (holdings/load-raw year))
        path (holdings/path year)]
    (report-prune result)
    (println (format "writing %s .." path))
    (flush)
    (holdings/write year (:filers result))
    (println (format "writing %s .. done!" path))
    (println (format "Wrote %s  (%d filers)" path (count (:filers result))))
    (flush)
    (:filers result)))

(defn print-filers
  "Build data/13f-index-<year>.edn, write data/holdings-raw-<year>.nippy from
  the daily archives that are already downloaded, prune duplicates into
  data/holdings-<year>.nippy, then print the filer table.

  `opts` is {:year 2026}."
  [{:keys [year]}]
  (let [year (require-year year)
        index (write-index! year)
        filers (load-holdings (:filings index))]
    (write-holdings! year filers)
    (print-table (prune-filers year))))
