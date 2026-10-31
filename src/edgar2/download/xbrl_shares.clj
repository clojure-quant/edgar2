(ns edgar2.download.xbrl-shares
  "Share counts from a companyfacts file, or from the latest annual filing.

  clj -X:xbrl-shares
  clj -X:xbrl-shares :ticker '\"SONY\"'
  clj -X:xbrl-shares :file '\"test/facts/COKE.json\"'"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.db.universe :as universe]
            [edgar2.download.dataset :as dataset]
            [edgar2.filing.xbrl :as xbrl]
            [edgar2.util :as util]
            [edgar2.util.core :as ucore]
            [edgar2.util.filing :as ufiling]
            [edgar2.util.map-progress :refer [map-progress]]
            [jsonista.core :as json])
  (:import [java.time LocalDate]))

(def default-file "test/facts/COKE.json")

(defn load-facts
  "Companyfacts map from a JSON file on disk."
  [path]
  (with-open [in (io/input-stream (io/file (str path)))]
    (json/read-value in json/keyword-keys-object-mapper)))

(def ^:private class-axes
  "Class-of-stock axes. An IFRS filer uses its own axis, not the US-GAAP one."
  [:us-gaap:StatementClassOfStockAxis
   :ifrs-full:ClassesOfShareCapitalAxis])

(defn- map-get
  "Value for `k` when map keys may be keywords or strings.

  A sorted map throws if probed with the other key type, which is how
  `edgar2.filing.xbrl/parse` stores units and dimensions."
  [m k]
  (when m
    (let [s (if (keyword? k) (name k) (str k))]
      (some (fn [[mk v]]
              (when (= s (if (keyword? mk) (name mk) (str mk)))
                v))
            m))))

(def ^:private entity-axis
  "A combined annual report tags each co-registrant's facts on this axis."
  :dei:LegalEntityAxis)

(defn- entity-member
  "Legal-entity member on `row`. Nil for the primary registrant, whose rows
  carry no entity axis."
  [row]
  (map-get (:dimensions row) entity-axis))

(defn- class-member
  "Value of the class-of-stock axis, or nil when the row has no class."
  [row]
  (some #(map-get (:dimensions row) %) class-axes))

(defn- local-name
  "Name after the last colon. A keyword's name may already contain one,
  because `:us-gaap:CommonClassAMember` has no namespace."
  [s]
  (let [s (if (keyword? s) (name s) (str s))
        i (str/last-index-of s ":")]
    (if i (subs s (inc i)) s)))

(defn- class-label
  "\"Class B\" for a ClassB, OrdinarySharesB, or ClassBShares member;
  \"Common Nonvoting\" or \"Common Voting\" for those common classes; and
  \"Common\" for no axis or any other member."
  [member]
  (let [n (local-name member)
        lower (str/lower-case n)
        common? (str/includes? lower "common")
        letter (or (second (re-find #"Class([A-Z])Member$" n))
                   (second (re-find #"Shares([A-Z])Member$" n))
                   (second (re-find #"([A-Z])Shares?Member$" n)))]
    (cond
      letter (str "Class " letter)
      (and common? (str/includes? lower "nonvoting")) "Common Nonvoting"
      (and common? (str/includes? lower "voting")) "Common Voting"
      :else "Common")))

(def ^:private exchange-axis-name "EntityListingsExchangeAxis")

(def ^:private stock-axis-names
  "Either axis marks a cover context as one class of stock."
  #{"StatementClassOfStockAxis" "ClassesOfShareCapitalAxis"})

(def ^:private debt-title-re
  "A cover title for notes, bonds, or debentures is not a share class."
  #"(?i)\b(?:notes?|bonds?|debentures?)\b")

(defn- share-class-context?
  "True when the cover context is a class of stock.

  A statement-class or IFRS share-capital axis is that class. No axes, or
  only the exchange axis, is the single common class. Debt and any other
  instrument are not. A title that names notes, bonds, or debentures is not
  a share even when the filer left the debt axis off."
  [dims title]
  (let [axes (into #{} (map local-name) (keys dims))]
    (and (not (re-find debt-title-re (str title)))
         (or (some axes stock-axis-names)
             (empty? (disj axes exchange-axis-name))))))

(defn- title-class
  "Class letter named in a cover title, e.g. \"Class A Common Stock\" or
  \"American Depositary Shares (each representing one B share)\". Nil when
  the title names no class."
  [title]
  (let [t (str title)]
    (or (second (re-find #"(?i)Class\s+([A-Z])\b" t))
        (second (re-find #"(?i)representing\s+(?:one|two|three|\d+)?\s*([A-Z])\s+share" t)))))

(defn- cover-class
  "Share class for one cover context: its class axis, else the class letter
  in the security title, else \"Common\"."
  [dims title]
  (if-let [member (class-member {:dimensions dims})]
    (class-label member)
    (if-let [letter (title-class title)]
      (str "Class " letter)
      "Common")))

(defn- cover-by-name
  [facts]
  (into {}
        (map (fn [fact]
               [(local-name (:qname fact)) (:val fact)]))
        facts))

(defn- class-markets
  "Ticker and exchange for each share class on the cover.

  Rows that share a context are one security. The first context for a
  class wins. A debt instrument, or a row that is not the parent's, is
  omitted."
  [cover parent?]
  (reduce
   (fn [m facts]
     (let [row (first facts)
           dims (:dimensions row)
           by (cover-by-name facts)
           title (get by "Security12bTitle")]
       (if (or (not (parent? row))
               (not (share-class-context? dims title)))
         m
         (let [class (cover-class dims title)
               ticker (not-empty (get by "TradingSymbol"))
               exchange (not-empty (get by "SecurityExchangeName"))]
           (if (or (contains? m class) (not (or ticker exchange)))
             m
             (assoc m class (cond-> {}
                              ticker (assoc :ticker ticker)
                              exchange (assoc :exchange exchange))))))))
   {}
   (vals (group-by :context cover))))

(defn- with-markets
  "Add :ticker and :exchange onto each class the cover lists as stock."
  [shares cover parent?]
  (let [markets (class-markets cover parent?)]
    (into {}
          (map (fn [[class row]]
                 [class (merge row (get markets class))]))
          shares)))

(defn- share-rows
  [concept]
  (or (map-get (:units concept) :shares)
      []))

(defn- latest-val
  "Value on the greatest :end. A later :filed breaks a tie."
  [rows]
  (when (seq rows)
    (:val (last (sort-by (juxt #(or (:end %) "") #(or (:filed %) "")) rows)))))

(defn- rows-by-class
  [rows]
  (group-by #(class-label (class-member %)) rows))

(defn- weighted-concept
  "Basic weighted-average shares.

  US-GAAP outstanding, then US-GAAP issued when the outstanding tag is
  absent, then IFRS weighted-average shares."
  [facts]
  (or (get-in facts [:facts :us-gaap :WeightedAverageNumberOfSharesOutstandingBasic])
      (get-in facts [:facts :us-gaap :WeightedAverageNumberOfSharesIssuedBasic])
      (get-in facts [:facts :ifrs-full :WeightedAverageShares])))

(defn- preference-class?
  "True for a preferred or preference share class, which is not common
  equity and so is not a share count for the common stock."
  [row]
  (let [n (some-> (class-member row) local-name str/lower-case)]
    (boolean (and n (re-find #"preference|preferred" n)))))

(defn- outstanding-rows
  "Share-count rows for the common stock.

  `dei:EntityCommonStockSharesOutstanding` when it has a share fact, then
  `us-gaap:SharesOutstanding`, then IFRS `ifrs-full:NumberOfSharesOutstanding`
  without preference classes. An empty higher-priority concept does not
  block the fallback."
  [facts]
  (or (not-empty (share-rows (get-in facts [:facts :dei :EntityCommonStockSharesOutstanding])))
      (not-empty (share-rows (get-in facts [:facts :us-gaap :SharesOutstanding])))
      (vec (remove preference-class?
                   (share-rows (get-in facts [:facts :ifrs-full :NumberOfSharesOutstanding]))))))

(defn- name-key
  "Lowercased letters and digits, so a DEI name can be compared with a
  member's local name."
  [s]
  (some-> s str str/lower-case (str/replace #"[^a-z0-9]" "") not-empty))

(defn- member-name-key
  "`wmb:TranscontinentalGasPipeLineCompanyLLCMember` → the company part,
  without the `Member` suffix."
  [member]
  (some-> member local-name name-key (str/replace #"member$" "")))

(defn- registrant-member?
  "True when `member` names the registrant in `entityName`.

  \"Williams Companies, Inc.\" matches `wmb:WilliamsCompaniesIncMember`, and
  so does \"The Williams Companies, Inc.\". A short member key is rejected
  so it cannot match everything."
  [entity-name member]
  (let [entity (name-key entity-name)
        member (member-name-key member)]
    (and entity member
         (>= (count member) 8)
         (or (str/includes? entity member)
             (str/includes? member entity)))))

(defn- share-facts
  "Outstanding and weighted share rows together. An entity that reports only
  one of the two still counts."
  [facts]
  (into []
        (concat (outstanding-rows facts)
                (share-rows (weighted-concept facts)))))

(defn reporting-entities
  "Number of legal entities that report share facts, or nil when every row
  belongs to one registrant.

  A combined 10-K lists the parent plus its subsidiary registrants, and tags
  the subsidiaries' rows with `dei:LegalEntityAxis`. The parent's rows have
  no entity axis. Three entities therefore return 3. An undimensioned parent
  and a parent-shaped member are one entity, not two."
  [facts]
  (let [members (distinct (map entity-member (share-facts facts)))
        named? #(registrant-member? (:entityName facts) %)]
    (when (some some? members)
      (if (some nil? members)
        (inc (count (remove named? (remove nil? members))))
        (count members)))))

(defn- parent-row?
  "Predicate for the rows that belong to the registrant.

  Rows without the axis are the parent. When every share row is dimensioned,
  the member named like `:entityName` wins; otherwise the first member in
  sort order, because a combined report lists the parent first. With an
  undimensioned parent, a member that also names the registrant is kept."
  [facts rows]
  (let [members (distinct (map entity-member rows))
        named? #(registrant-member? (:entityName facts) %)]
    (if-not (some some? members)
      (constantly true)
      (if (some nil? members)
        (fn [row]
          (let [member (entity-member row)]
            (or (nil? member) (named? member))))
        (let [parent (or (some #(when (named? %) %) members)
                         (first (sort-by str members)))]
          (fn [row] (= parent (entity-member row))))))))

(defn share-summary
  "Latest weighted-average and shares outstanding by common-stock class.

  `facts` is a companyfacts map. Outstanding shares are
  `:dei/EntityCommonStockSharesOutstanding`, then
  `:us-gaap/SharesOutstanding` when that tag has no share facts, then
  `:ifrs-full/NumberOfSharesOutstanding` without preference classes.
  Weighted shares are
  `:us-gaap/WeightedAverageNumberOfSharesOutstandingBasic`, then
  `:us-gaap/WeightedAverageNumberOfSharesIssuedBasic` when that tag is
  absent, then `:ifrs-full/WeightedAverageShares`.
  A row with no class axis (`:us-gaap:StatementClassOfStockAxis` or
  `:ifrs-full:ClassesOfShareCapitalAxis`) is \"Common\". A ClassA, ClassB,
  OrdinarySharesB, or ClassBShares member is \"Class A\", \"Class B\", and
  so on; a nonvoting or voting common member is \"Common Nonvoting\" or
  \"Common Voting\"; any other member is \"Common\". Each figure is the
  observation with the greatest `:end`.

  When several entities report (`dei:LegalEntityAxis`), only the parent's
  rows are used. See `reporting-entities` for how many there are.

  When `facts` has `:cover` (from `edgar2.filing.xbrl/parse`), each class
  the cover lists as stock also gets `:ticker` and `:exchange`. A debt
  line, such as senior notes, is not a share class and is left out."
  [facts]
  (let [rows (share-facts facts)
        parent? (parent-row? facts rows)
        outstanding (rows-by-class (filterv parent? (outstanding-rows facts)))
        weighted (rows-by-class
                  (filterv parent? (share-rows (weighted-concept facts))))
        classes (->> (concat (keys outstanding) (keys weighted))
                     distinct
                     (sort-by #(if (= % "Common") " " %)))]
    (with-markets
     (into {}
           (map (fn [class]
                  [class {:weighted (latest-val (get weighted class))
                          :outstanding (latest-val (get outstanding class))}]))
           classes)
     (:cover facts)
     parent?)))

(defn- ticker-name
  [ticker]
  (when (some? ticker)
    (let [s (str/upper-case
             (cond
               (string? ticker) ticker
               (keyword? ticker) (name ticker)
               :else (str ticker)))]
      (when-not (str/blank? s) s))))

(defn- instance-name
  "Extracted XBRL instance for a primary document.
  `form40f.htm` is `form40f_htm.xml`."
  [primary]
  (when-let [[_ stem] (re-find #"(?i)^(.+)\.html?$" (str primary))]
    (str stem "_htm.xml")))

(defn- instance-text
  "Extracted instance for `filing`, or nil when SEC has no such document.
  Its contexts and units resolve cover facts that were left without them."
  [filing]
  (when-let [name (instance-name (:primaryDocument filing))]
    (try
      (e/filing-document filing name)
      (catch clojure.lang.ExceptionInfo e
        (if (= 404 (:status (ex-data e)))
          nil
          (throw e))))))

(defn share-summary-cik
  "Latest annual filing for `cik`, parsed with `edgar2.filing.xbrl/parse-all`.

  The filing is the newest 10-K, 20-F, or 40-F. When the cover links an
  audited-statement exhibit (`_d2` or `ex99d2`), that HTML is parsed with
  the cover so its facts can use the cover's contexts. The extracted
  instance (`form40f_htm.xml`) is included when SEC has one, so a cover
  fact can use a context that the inline HTML left out.
  Each share class also has the cover `:ticker` and `:exchange`. A listed
  note or bond is not a share class and is left out. When subsidiaries are
  co-registrants (`dei:LegalEntityAxis`), only the parent's share counts are
  used and `:entities` holds how many entities report.
  Returns `{:cik :filing-date :form-type :shares}`, plus `:entities` when
  the filing carries an entity axis."
  [cik]
  (ucore/ensure-identity!)
  (let [filing (ufiling/latest-annual-filing cik)]
    (when-not filing
      (throw (ex-info "no annual filing" {:cik cik})))
    (let [html (e/html filing)
          exhibit (xbrl/statement-exhibit-name html)
          texts (->> [html
                      (when exhibit (e/filing-document filing exhibit))
                      (instance-text filing)]
                     (keep identity)
                     vec)
          parsed (xbrl/parse-all texts {:accn (:accessionNumber filing)
                                        :filed (:filingDate filing)
                                        :form (:form filing)})
          entities (reporting-entities parsed)]
      (cond-> {:cik cik
               :filing-date (:filingDate filing)
               :form-type (:form filing)
               :shares (share-summary parsed)}
        entities
        (assoc :entities entities)))))

(defn share-summary-ticker
  "CIK for `ticker`, then `share-summary-cik`, with `:ticker` added."
  [ticker]
  (ucore/ensure-identity!)
  (let [ticker (ticker-name ticker)
        cik (util/resolve-filer2 ticker)]
    (when-not cik
      (throw (ex-info "no CIK for ticker" {:ticker ticker})))
    (assoc (share-summary-cik cik) :ticker ticker)))

(defn- print-concept
  [facts taxonomy concept]
  (println)
  (println (str (name taxonomy) ":" (name concept)))
  (pprint/pprint (get-in facts [:facts taxonomy concept])))

(defn- run-file
  [file]
  (let [path (or file default-file)
        facts (load-facts path)]
    (println (:entityName facts) " cik " (:cik facts))
    (println "source " path)
    (print-concept facts :dei :EntityCommonStockSharesOutstanding)
    (print-concept facts :us-gaap :WeightedAverageNumberOfSharesOutstandingBasic)
    (println)
    (println "share-summary")
    (pprint/pprint (share-summary facts))
    nil))

(def share-classes-path
  "Written by `run` when no ticker is passed."
  "data/share-classes.edn")

(defn- summary-for-universe-row
  "Share summary for one universe-facts row. A failure is a row with :error."
  [{:keys [ticker cik]}]
  (try
    (assoc (share-summary-cik cik) :ticker ticker)
    (catch Throwable ex
      (let [msg (or (ex-message ex) (str ex))]
        (println (format "  skip %s %s (%s)" ticker cik msg))
        (flush)
        {:ticker ticker :cik cik :error msg}))))

(defn run-universe
  "Share summary for every ticker in data/universe-facts.edn.

  Progress prints every 100 tickers. The file is written after the last one."
  []
  (ucore/ensure-identity!)
  (ucore/enable-cache)
  (let [rows (->> (universe/load-universe)
                  (filter #(not (str/blank? (str (:ticker %)))))
                  vec)
        n (count rows)]
    (println (format "share classes  %d tickers  → %s" n share-classes-path))
    (flush)
    (let [out (map-progress
               (fn [i total result]
                 (format "  %d/%d  %s" i total (:ticker result)))
               summary-for-universe-row
               rows)]
      (dataset/write-dataset! share-classes-path
                              {:download-date (str (LocalDate/now))}
                              out)
      (println (format "Wrote %s  (%d tickers)" share-classes-path (count out)))
      out)))

(defn run
  "With `:ticker`, print `share-summary-ticker`. With `:file`, load that
  companyfacts JSON. With neither, summarize every universe ticker into
  data/share-classes.edn."
  ([] (run {}))
  ([{:keys [file ticker]}]
   (cond
     (ticker-name ticker)
     (let [summary (share-summary-ticker ticker)]
       (pprint/pprint summary)
       summary)

     file
     (run-file file)

     :else
     (run-universe))))
