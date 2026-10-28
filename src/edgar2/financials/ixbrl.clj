(ns edgar2.financials.ixbrl
  "Inline XBRL facts from a filing companyfacts has not ingested yet.

  Contexts often live in the cover HTML and the statement numbers in an
  exhibit such as as-20260630_d2.htm. Both are read and joined on context id."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.core :as ec]
            [edgar.filing :as ef])
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

(defn- local-name
  [qname]
  (let [s (str qname)]
    (subs s (inc (or (str/last-index-of s ":") -1)))))

(defn fact-seq
  "Undimensioned ix:nonFraction facts whose context id is in `contexts`."
  [html contexts]
  (keep (fn [[_ attrs text]]
          (let [cref (ix-attr attrs "contextRef")
                ctx (get contexts cref)
                concept (local-name (ix-attr attrs "name"))
                scale (try (parse-long (or (ix-attr attrs "scale") "0"))
                           (catch Exception _ nil))
                val (when (and ctx scale concept)
                      (ix-number text scale (ix-attr attrs "sign")))]
            (when val
              {:concept concept
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

(defn filing-facts
  "Undimensioned facts from a 6-K's inline XBRL.

  Each map is {:concept :start :end :unit :val :accn :form :filed}.
  A later _d2 exhibit replaces the same fact from the cover."
  [filing]
  (let [names (or (seq (cached-html-names filing))
                  (html-names (ef/filing-index filing)))
        htmls (mapv #(read-doc! filing %) names)
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
