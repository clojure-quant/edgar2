(ns edgar2.financials.ixbrl
  "Inline XBRL facts from a filing companyfacts has not ingested yet.

  Contexts often live in the cover HTML and the statement numbers in an
  exhibit such as as-20260630_d2.htm. Both are read and joined on context id.

  An annual report can be in the same state. GreenPower's 2026 20-F is
  inline XBRL, and companyfacts kept only the cover-page share count.
  The year ended 2025-03-31 is a comparative column of that filing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.core :as ec]
            [edgar.filing :as ef]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds])
  (:import [java.math BigDecimal]))

(def ^:private filing-dir "data/filings")

(defn- flag-on?
  "SEC submissions store flags as 1/0. nil when the field is absent."
  [v]
  (cond
    (nil? v) nil
    (true? v) true
    (false? v) false
    (and (number? v) (== 1 (long v))) true
    (and (number? v) (== 0 (long v))) false
    :else (let [s (str/lower-case (str v))]
            (cond
              (#{"1" "true"} s) true
              (#{"0" "false"} s) false
              :else nil))))

(defn statement-package?
  "True when this 6-K should be parsed for quarterly numbers.

  `isXBRLNumeric` 1 is the financial-statement package. Explicit 0 is a
  press release filed the same day. Older submissions omit the flag; then
  an inline-XBRL 6-K is used only when its report date is after the latest
  period already present, so historical earnings releases are left alone."
  [filing latest-end]
  (when (str/starts-with? (str (:form filing)) "6-K")
    (let [numeric (flag-on? (:isXBRLNumeric filing))]
      (cond
        (true? numeric) true
        (false? numeric) false
        (not (true? (flag-on? (:isInlineXBRL filing)))) false
        (str/blank? (str latest-end)) true
        :else (pos? (compare (str (:reportDate filing)) (str latest-end)))))))

(defn- ix-attr
  [attrs k]
  (second (re-find (re-pattern (str k "=\"([^\"]*)\"")) (str attrs))))

(defn- dimensional?
  [body]
  (boolean (re-find #"<(?:[\w-]+:)?(?:explicitMember|typedMember)\b" (str body))))

(defn contexts
  "Context id → {:start :end}. Dimensional contexts are omitted."
  [html]
  (into {}
        (for [[_ id body] (re-seq #"(?s)<xbrli:context id=\"([^\"]+)\">(.*?)</xbrli:context>"
                                  (str html))
              :when (not (dimensional? body))]
          [id {:start (second (re-find #"<xbrli:startDate>([^<]+)" body))
               :end (or (second (re-find #"<xbrli:endDate>([^<]+)" body))
                        (second (re-find #"<xbrli:instant>([^<]+)" body)))}])))

(defn- ix-number
  "Number in the fact body, times 10^scale. A sign attribute flips it.
  BigDecimal keeps 1,632.6 × 10^6 at 1632600000 rather than a short double."
  [text scale sign]
  (let [raw (-> (str text) str/trim
                (str/replace "," "")
                (str/replace #"^\((.+)\)$" "-$1"))]
    (when (re-matches #"-?\d+(?:\.\d+)?" raw)
      (let [v (bigdec raw)
            v (if (= sign "-") (.negate v) v)
            scaled (.movePointRight ^BigDecimal v (long (or scale 0)))]
        (double scaled)))))

(defn- split-qname
  "Taxonomy prefix and local name from an inline XBRL `name`."
  [qname]
  (let [s (str qname)
        i (str/last-index-of s ":")]
    (if (and i (pos? i))
      [(subs s 0 i) (subs s (inc i))]
      [nil s])))

(defn fact-seq
  "Undimensioned ix:nonFraction facts whose context id is in `contexts`."
  [html contexts]
  (keep (fn [[_ attrs text]]
          (let [cref (ix-attr attrs "contextRef")
                ctx (get contexts cref)
                [taxonomy concept] (split-qname (ix-attr attrs "name"))
                scale (try (parse-long (or (ix-attr attrs "scale") "0"))
                           (catch Exception _ nil))
                val (when (and ctx scale (not (str/blank? concept)))
                      (ix-number text scale (ix-attr attrs "sign")))]
            (when val
              {:taxonomy taxonomy
               :concept concept
               :start (:start ctx)
               :end (:end ctx)
               :unit (some-> (ix-attr attrs "unitRef") str/upper-case)
               :val val})))
        (re-seq #"<ix:nonFraction\b([^>]*)>([^<]*)" (str html))))

(defn- html-name?
  [name]
  (boolean (re-find #"(?i)\.html?$" (str name))))

(defn- viewer-name?
  "SEC viewer pages (R1.htm) and the index itself are not the filing."
  [name]
  (boolean (re-find #"(?i)(?:^R\d+\.html?$|-index)" (str name))))

(defn- statement-name?
  "The _d2 exhibit carries the statements; it is read after the cover."
  [name]
  (boolean (re-find #"(?i)_d2\.html?$" (str name))))

(defn- html-names
  [index]
  (->> (:files index)
       (map :name)
       (filter html-name?)
       (remove viewer-name?)
       (sort-by (fn [name] [(if (statement-name? name) 1 0) (str name)]))
       vec))

(defn- accession-dir
  [form cik accession]
  (io/file filing-dir (str form) (str cik) (str accession)))

(defn- cache-file
  [form cik accession doc]
  (io/file (accession-dir form cik accession) (str doc)))

(defn- read-doc!
  "HTML for one filing document, from data/filings or the SEC archives."
  [filing doc]
  (let [{:keys [form cik accessionNumber]} filing
        f (cache-file form cik accessionNumber doc)]
    (cond
      (.isFile f) (slurp f :encoding "UTF-8")
      :else (let [html (ec/edgar-get (ef/filing-doc-url filing doc) :raw? true)]
              (.mkdirs (.getParentFile f))
              (spit f html :encoding "UTF-8")
              html))))

(defn- cached-html-names
  [filing]
  (let [{:keys [form cik accessionNumber]} filing
        dir (accession-dir form cik accessionNumber)]
    (when (.isDirectory dir)
      (->> (.listFiles dir)
           (remove nil?)
           (map #(.getName %))
           (filter html-name?)
           (remove viewer-name?)
           (sort-by (fn [name] [(if (statement-name? name) 1 0) name]))
           vec))))

(defn- facts-from-docs
  "Undimensioned facts from `names`, joined on context id.

  A later document replaces the same fact. Each map is
  {:taxonomy :concept :start :end :unit :val :accn :form :filed}."
  [filing names]
  (let [htmls (mapv #(read-doc! filing %) names)
        ctx (reduce merge {} (map contexts htmls))
        rows (mapcat #(fact-seq % ctx) htmls)]
    (->> rows
         (reduce (fn [m fact]
                   (assoc m [(:concept fact) (str (:start fact)) (str (:end fact))
                             (str (:unit fact))]
                          fact))
                 {})
         vals
         (map #(assoc %
                      :accn (:accessionNumber filing)
                      :form (str (:form filing))
                      :filed (:filingDate filing)))
         vec)))

(defn filing-facts
  "Undimensioned facts from a 6-K's inline XBRL.

  Each map is {:taxonomy :concept :start :end :unit :val :accn :form :filed}.
  A later _d2 exhibit replaces the same fact from the cover."
  [filing]
  (facts-from-docs filing (or (seq (cached-html-names filing))
                              (html-names (ef/filing-index filing)))))

(defn annual-report-form?
  "10-K, 20-F, 40-F, including amendments."
  [form]
  (boolean (re-find #"^(10-K|20-F|40-F)" (str form))))

(defn inline-annual?
  "True when this filing is an inline-XBRL annual report."
  [filing]
  (and (annual-report-form? (:form filing))
       (true? (flag-on? (:isInlineXBRL filing)))))

(defn- annual-window-row?
  [row]
  (and (annual-report-form? (:form row))
       (facts-ds/annual-duration? row)))

(defn pending-annual-filings
  "Inline annual reports whose ~12-month window is missing from companyfacts.

  `rows` are companyfacts fact rows. A filing is pending when it is inline
  XBRL, its report date is after the latest annual period-end already
  present, and its accession has no ~12-month fact. Companyfacts can still
  list that accession: GreenPower's 2026 20-F is there as the cover-page
  share count only. When companyfacts has no annual window, only the newest
  inline annual is pending."
  [filings rows]
  (let [known (into #{}
                    (keep (fn [row]
                            (when (annual-window-row? row)
                              (not-empty (str (:accn row))))))
                    rows)
        latest (->> rows
                    (filter annual-window-row?)
                    (map #(str (:end %)))
                    (remove str/blank?)
                    sort
                    last)
        candidates (->> filings
                        (filter (fn [filing]
                                  (and (inline-annual? filing)
                                       (not (contains? known (str (:accessionNumber filing))))
                                       (or (nil? latest)
                                           (pos? (compare (str (:reportDate filing))
                                                          latest))))))
                        (sort-by #(str (:filingDate %)) #(compare %2 %1))
                        vec)]
    (if (some? latest)
      candidates
      (vec (take 1 candidates)))))

(defn- ended-by-filing?
  [fact]
  (not (pos? (compare (str (:end fact)) (str (:filed fact))))))

(defn annual-statement-facts
  "Annual-duration facts and instants on those period-ends, tagged `:fp` FY.

  Short windows (a share issuance, a quarter) are omitted. `:fy` is the
  period-end year, so a comparative column is labeled as its own year.
  A window that ends after the filing date is omitted."
  [facts]
  (let [windows (filterv #(and (facts-ds/annual-duration? %)
                               (ended-by-filing? %))
                         facts)
        ends (set (map #(str (:end %)) windows))
        instants (filterv (fn [fact]
                            (and (str/blank? (str (:start fact)))
                                 (contains? ends (str (:end fact)))
                                 (ended-by-filing? fact)))
                          facts)]
    (mapv (fn [fact]
            (let [year (some-> (facts-ds/parse-date (:end fact)) .getYear)]
              (assoc fact :fp "FY" :fy (or (:fy fact) year))))
          (concat windows instants))))

(defn- exhibit-name?
  [name]
  (boolean (re-find #"(?i)exhibit" (str name))))

(defn- annual-doc-names
  "Primary annual document, plus a `_d2` statement exhibit when the
  filing has one. Contract exhibits are not the statements."
  [filing]
  (let [primary (some-> (:primaryDocument filing) str str/trim not-empty)
        indexed (try (html-names (ef/filing-index filing))
                     (catch Exception _ []))
        d2 (filter statement-name? indexed)
        cover (when-not primary
                (first (remove exhibit-name? indexed)))]
    (vec (distinct (keep not-empty (concat [primary cover] d2))))))

(defn annual-filing-facts
  "Annual-statement facts from an annual report's inline XBRL.

  Reads the primary document and a `_d2` exhibit. Same maps as
  `filing-facts`, with `:fp` FY and `:fy` set."
  [filing]
  (annual-statement-facts (facts-from-docs filing (annual-doc-names filing))))

(defn with-pending-annual-ixbrl
  "Companyfacts plus inline XBRL from annual reports that never fully
  landed in companyfacts.

  GreenPower's 2026 20-F is one of these. Its comparative column is the
  year ended 2025-03-31, which companyfacts does not have, and the 2025
  20-F itself was filed without XBRL. A failure leaves `dss` unchanged."
  [dss cik]
  (try
    (let [rows (when dss (vec (ds/rows dss {:nil-missing? true})))
          pending (pending-annual-filings
                   (e/filings cik :include-amends? true :limit 1000)
                   rows)
          extra (mapcat (fn [filing]
                          (try
                            (annual-filing-facts filing)
                            (catch Exception _
                              [])))
                        pending)]
      (if (seq extra)
        (ds/->dataset (concat rows extra)
                      {:dataset-name "facts-with-annual-ixbrl"})
        dss))
    (catch Exception _
      dss)))
