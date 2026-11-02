(ns edgar2.filing.xbrl-shares
  "Extract `dei:EntityCommonStockSharesOutstanding` from annual filings and
  reduce the facts to one share count.

  `check` labels facts by dimension only: an undimensioned fact is
  \"CommonStockTotal\", a fact on `ClassesOfShareCapitalAxis` or
  `StatementClassOfStockAxis` keeps its member keyword, and every other
  dimension is \"UnknownClass\"."
  (:require
   [edgar2.filing.xbrl-loader :as xbrl-loader]
   [edgar2.filing.xbrl :as xbrl]
   [edgar2.web.filing :as wf]))

(defn get-shares-outstanding
  "The `dei:EntityCommonStockSharesOutstanding` facts in `summary`."
  [summary]
  (->> summary
       (xbrl/facts-with-concepts)
       (filter #(and (= :dei (:taxonomy %))
                     (= :EntityCommonStockSharesOutstanding (:concept %))))
       (map #(select-keys % [:concept :value :dimension]))))

(defn fetch-share-data
  "Share facts from `ticker`'s latest 10-K, 20-F, or 40-F."
  [ticker]
  (-> (wf/annual-report-file ticker)
      (.getPath)
      (xbrl-loader/load-documents)
      (xbrl/overview)
      (get-shares-outstanding)))

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

(defn transform-share-row [{:keys [value] :as row}]
  [(determine-class row) value])

(defn outstanding
  "Class key → value. A later fact with the same key wins."
  [rows]
  (->> rows
       (map transform-share-row)
       (remove #(nil? (first %)))
       (into {})))

(defn check
  "Class key → shares outstanding from `ticker`'s latest annual filing."
  [ticker]
  (-> ticker
      fetch-share-data
      outstanding))

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
