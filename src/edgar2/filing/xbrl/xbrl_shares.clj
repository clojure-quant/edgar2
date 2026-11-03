(ns edgar2.filing.xbrl.xbrl-shares
  "Extract `dei:EntityCommonStockSharesOutstanding` from annual filings and
  reduce the facts to one share count.

  `check` labels facts by dimension only: an undimensioned fact is
  \"CommonStockTotal\", a fact on `ClassesOfShareCapitalAxis` or
  `StatementClassOfStockAxis` keeps its member keyword, and every other
  dimension is \"UnknownClass\"."
  (:require
   [clojure.string :as str]
   [edgar2.filing.xbrl-loader :as xbrl-loader]
   [edgar2.filing.xbrl-overview :as xbrl-overview]
   [edgar2.web.filing :as wf])
  (:import [java.time LocalDate]))

(defn load-filing-facts [file-or-url]
  (-> file-or-url
      (xbrl-loader/load-documents)
      (xbrl-overview/overview)
      (xbrl-overview/facts-with-concepts)))

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

(defn class-table? [fact]
  (or
   (shares-outstanding? fact)
   (weighted-share? fact)
   (diluted-share? fact)))

(defn ticker-table? [fact]
  (or (ticker? fact)
      (title? fact)
      (exchange? fact)))


;; share-class 

(defn fact-dimension
  "The axis→member map joined onto a fact's `:entity`, or nil when the fact
  has no entity or no dimensions."
  [fact]
  (get-in fact [:entity :dimension]))

(defn determine-class
  "Class key for a fact: the undimensioned total, a class-axis member, or
  \"UnknownClass\"."
  [fact]
  (let [dimension (fact-dimension fact)]
    (cond
      (empty? dimension)
      "CommonStockTotal"

      (contains? dimension :ifrs-full/ClassesOfShareCapitalAxis)
      (get dimension :ifrs-full/ClassesOfShareCapitalAxis)

      (contains? dimension :us-gaap/StatementClassOfStockAxis)
      (get dimension :us-gaap/StatementClassOfStockAxis)

      (contains? dimension :dei/LegalEntityAxis)
      (get dimension :dei/LegalEntityAxis)

      :else
      "UnknownClass")))

(defn fact-for-dei-legal-entity? [{:keys [entity] :as _fact}]
  ; :dei/LegalEntityAxis means the fact is for a different entity.
  ; so we dont want it
  ; TDAC reports share classes under :dei/LeegalEntityAxis
  ; we added an escape hatch, because the values are normally
  ; namespaced to contain a issuer specific namespace
  (and (contains? (:dimension entity) :dei/LegalEntityAxis)
       (let [v (get-in entity [:dimension :dei/LegalEntityAxis])
             n (namespace v)]
         (not (= n "us-gaap")))))
;(namespace :us-gaap/CommonClassAMember)

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
       (map determine-class)
       (distinct)))

(defn periods [facts]
  (->> facts
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


(defn facts->map [fields facts]
  (->> fields
       (map (fn [[k v]]
              [k (->> facts
                      (filter v)
                      (map :value)
                      (remove nil?)
                      (into [])
                      first)]))
       (into {})))

;; classes

(def fields-classes
  {:outstanding shares-outstanding?
   :weighted weighted-share?
   :diluted diluted-share?})

(defn tabulate-classes [facts]
  (->> facts
       (group-by determine-class)
       (map (fn [[k v]]
              [k (facts->map fields-classes v)]))
       (into {})))

;; tickers

(def ^:private debt-title-re
  "A cover title for notes, bonds, or debentures is not a share class."
  #"(?i)\b(?:notes?|bonds?|debentures?)\b")

(defn debt-title? [fact]
  (re-find debt-title-re (str (:title fact))))

(def fields-tickers
  {:ticker ticker?
   :exchange exchange?
   :title title?})


(defn tabulate-tickers [facts]
  (->> facts
       (group-by #(or (fact-dimension %) {}))
       (map (fn [[k v]]
              (assoc (facts->map fields-tickers v) :class k)))
       (remove debt-title?)
       (remove (fn [{:keys [ticker title]}]
                 (and (str/blank? (str ticker))
                      (str/blank? (str title)))))
       (into [])))


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
       (map #(select-keys % [:concept :value :entity]))))



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
  [facts-with-concepts]
  (let [facts-ticker-table (->> facts-with-concepts
                                (filter ticker-table?)
                                (remove-old-periods))
        facts-class-table (->> facts-with-concepts
                               (filter class-table?)
                               (remove fact-for-dei-legal-entity?) ; UAL reports shares for multiple entities
                               (remove-old-periods))]
    {:end-date (get-dei-fact facts-with-concepts :DocumentPeriodEndDate)
     :cik  (get-dei-fact facts-with-concepts :EntityCentralIndexKey)
     :company-name  (get-dei-fact facts-with-concepts :EntityRegistrantName)
     ;:facts-all (count facts-with-concepts)
     ;:facts-ticker (count facts-ticker-table)
     ;:facts-class (count facts-class-table)

     ;:facts-first (first facts-with-concepts)
     ;:classes (classes facts-class-table)
     ;:periods (periods facts-class-table)
     :shares (tabulate-classes facts-class-table)
     :tickers (tabulate-tickers facts-ticker-table)}))

;; ticker based version

(defn ticker->url-file
  "Path of `ticker`'s latest annual filing, or nil when there is none."
  [ticker]
  (some-> (wf/annual-report-file ticker)
          (.getPath)))

(defn share-summary-ticker [ticker]
  (if-let [file (ticker->url-file ticker)]
    (-> file
        (load-filing-facts)
        (share-summary))
    {:ticker ticker :error "no annual filing found for ticker"}))

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
    (-> shares first val :outstanding)

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
          classes  (dissoc classes :dei/AdrMember aggregate)]
      (when (seq classes)
        (->> classes
             vals
             (map :outstanding)
             (filter number?)
             (reduce + 0))))))
