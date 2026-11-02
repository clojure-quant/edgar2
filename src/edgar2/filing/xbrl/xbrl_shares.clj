(ns edgar2.filing.xbrl.xbrl-shares
  "Extract `dei:EntityCommonStockSharesOutstanding` from annual filings and
  reduce the facts to one share count.

  `check` labels facts by dimension only: an undimensioned fact is
  \"CommonStockTotal\", a fact on `ClassesOfShareCapitalAxis` or
  `StatementClassOfStockAxis` keeps its member keyword, and every other
  dimension is \"UnknownClass\"."
  (:require
   [edgar2.filing.xbrl-loader :as xbrl-loader]
   [edgar2.filing.xbrl :as xbrl]
   [edgar2.web.filing :as wf])
  (:import [java.time LocalDate]
           [java.time.temporal ChronoUnit]))

(defn fetch-share-data
  "returns facts with concepts for a ticker."
  [ticker]
  (-> (wf/annual-report-file ticker)
      (.getPath)
      (xbrl-loader/load-documents)
      (xbrl/overview)
      (xbrl/facts-with-concepts)))

;; predicates for concept

(defn weighted-share? [{:keys [taxonomy concept]}]
  (or
   (and (= :us-gaap taxonomy)
        (= :WeightedAverageNumberOfSharesOutstandingBasic concept))
   (and (= :us-gaap taxonomy)
        (= :WeightedAverageNumberOfSharesIssuedBasic concept))
   (and (= :us-gaap taxonomy)
        (= :WeightedAverageLimitedPartnershipUnitsOutstanding concept))
   (and (= :ifrs-full taxonomy)
        (= :WeightedAverageShares concept))))

(defn diluted-share? [{:keys [taxonomy concept]}]
  (or
   (and (= :us-gaap taxonomy)
        (= :WeightedAverageNumberOfDilutedSharesOutstanding concept))
   (and (= :us-gaap taxonomy)
        (= :WeightedAverageLimitedPartnershipUnitsOutstandingDiluted concept))
   (and (= :us-gaap taxonomy)
        (= :WeightedAverageNumberDilutedSharesOutstandingAdjustment concept))
   (and (= :ifrs-full taxonomy)
        (= :AdjustedWeightedAverageShares concept))))

(defn ticker? [{:keys [taxonomy concept]}]
  (and (= :dei taxonomy)
       (= :TradingSymbol concept)))

(defn exchange? [{:keys [taxonomy concept]}]
  (and (= :dei taxonomy)
       (= :SecurityExchangeName concept)))

(defn title? [{:keys [taxonomy concept]}]
  (and (= :dei taxonomy)
       (= :Security12bTitle concept)))

(defn shares-outstanding? [{:keys [taxonomy concept]}]
  (and (= :dei taxonomy)
       (= :EntityCommonStockSharesOutstanding concept)))

(defn relevant? [fact]
  (or (ticker? fact)
      (title? fact)
      (exchange? fact)
      (shares-outstanding? fact)
      (weighted-share? fact)
      (diluted-share? fact)))


;; share-class 

(defn determine-class
  "Class key for a fact: the undimensioned total, a class-axis member, or
  \"UnknownClass\"."
  [{:keys [dimension]}]
  (cond
    (empty? dimension)
    "CommonStockTotal"

    (contains? dimension :ifrs-full/ClassesOfShareCapitalAxis)
    (get dimension :ifrs-full/ClassesOfShareCapitalAxis)

    (contains? dimension :us-gaap/StatementClassOfStockAxis)
    (get dimension :us-gaap/StatementClassOfStockAxis)

    :else
    "UnknownClass"))

;; filing info 

(defn dei-concept? [target-concept {:keys [taxonomy concept]}]
  (and (= :dei taxonomy)
       (= target-concept concept)))

(defn get-dei-fact [facts target-concept]
  (->> facts
       (filter (partial dei-concept? target-concept))
       first
       :value))

;; tabulateable categories: classes and periods

(defn classes [facts]
  (->> facts
       (filter relevant?)
       (map determine-class)
       (distinct)))

(defn periods [facts]
  (->> facts
       (filter relevant?)
       (map :period)
       (distinct)))

;; remove facts with old intervals

(defn last-interval [periods]
  (->> periods
       (filter :end)
       (sort-by :end)
       last))

(defn last-interval-or-instant [last-interval period]
  (if (:instant period)
    true
    (= (:end period) (:end last-interval))))

(defn remove-old-periods [facts]
  (let [p (periods facts)
        li (last-interval p)]
    (filter #(last-interval-or-instant li (:period %)) facts)))

;; tabulation

(def fields
  {:outstanding shares-outstanding?
   :weighted weighted-share?
   :diluted diluted-share?
   :ticker ticker?
   :exchange exchange?
   :title title?})

(defn facts->map [facts]
  (->> fields
       (map (fn [[k v]]
              [k (->> facts
                      (filter v)
                      (map :value)
                      (into [])
                      first
                      )]))
       (into {})))

(into {} [[:a 1] [:b 2]])

(into [] '(1 2))

(defn tabulate-classes [facts]
  (->> facts
       (group-by determine-class)
       (map (fn [[k v]]
              [k (facts->map v)]))
       (into {})))

(defn parse-date
  [d]
  (when d
    (try (LocalDate/parse (str d))
         (catch Exception _ nil))))



(defn filter-shares-outstanding
  "The `dei:EntityCommonStockSharesOutstanding` facts in `summary`."
  [facts-with-concepts]
  (->> facts-with-concepts
       (filter shares-outstanding?)
       (map #(select-keys % [:concept :value :dimension]))))



(defn transform-share-row [{:keys [value] :as row}]
  [(determine-class row) value])

(defn outstanding
  "Class key → value. A later fact with the same key wins."
  [facts-with-concepts]
  (->> facts-with-concepts
       (filter-shares-outstanding)
       (map transform-share-row)
       (remove #(nil? (first %)))
       (into {})))



;; summary

(defn share-summary
  "Class key → shares outstanding from `ticker`'s latest annual filing."
  [ticker]
  (let [facts-with-concepts (fetch-share-data ticker)
        facts-relevant (filter relevant? facts-with-concepts)
        facts-most-recent (remove-old-periods facts-relevant)]
    {:end-date (get-dei-fact facts-with-concepts :DocumentPeriodEndDate)
     :cik  (get-dei-fact facts-with-concepts :EntityCentralIndexKey)
     :company-name  (get-dei-fact facts-with-concepts :EntityRegistrantName)

     :facts-all (count facts-with-concepts)
     :facts-relevant (count facts-relevant)
     :facts-most-recent (count facts-most-recent)

     :facts-first (first facts-with-concepts)
     :classes (classes facts-relevant)
     :periods (periods facts-relevant)
     :shares (tabulate-classes facts-most-recent)}))


;; db helper

(defn shares-outstanding
  "Number of shares outstanding from a `check` result.

  One class returns its value. Otherwise the undimensioned total is dropped;
  a `:us-gaap/CommonStockMember`, `:ifrs-full/OrdinarySharesMember`, or
  \"UnknownClass\" value that equals the sum of the other classes is treated
  as their aggregate and dropped; and `:dei/AdrMember`, an ADR count rather
  than a share class, is dropped. The remaining class values are summed."
  [shares]
  (cond
    (empty? shares)
    nil

    (= 1 (count shares))
    (val (first shares))

    :else
    (let [classes (dissoc shares "CommonStockTotal")
          without-adr (dissoc classes :dei/AdrMember)
          aggregate (some (fn [[k v]]
                            (when (and (or (= k :us-gaap/CommonStockMember)
                                           (= k :ifrs-full/OrdinarySharesMember)
                                           (= k "UnknownClass"))
                                       (number? v)
                                       (let [others (vals (dissoc without-adr k))]
                                         (and (seq others)
                                              (every? number? others)
                                              (== v (reduce + 0 others)))))
                              k))
                          (sort-by (comp str key) classes))
          values (vals (dissoc classes aggregate :dei/AdrMember))]
      (when (and (seq values) (every? number? values))
        (reduce + 0 values)))))
