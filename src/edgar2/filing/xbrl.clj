(ns edgar2.filing.xbrl
  "Low-level reader for one inline XBRL filing document.

  Provides the jsoup parse, element and attribute lookup, period,
  continuation, and numeric-value helpers shared by the companyfacts
  builder (`edgar2.filing.xbrl-facts`) and the flat
  `edgar2.filing.xbrl-overview` view.

  Parsed with jsoup. A filing is HTML, and a strict XML read rejects the
  ones that are not well-formed."
  (:require [clojure.string :as str])
  (:import [java.math BigDecimal]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Element]
           [org.jsoup.parser Parser]))

(def ^:private word-value
  {"zero" 0 "no" 0 "none" 0
   "one" 1 "two" 2 "three" 3 "four" 4 "five" 5
   "six" 6 "seven" 7 "eight" 8 "nine" 9
   "ten" 10 "eleven" 11 "twelve" 12 "thirteen" 13
   "fourteen" 14 "fifteen" 15 "sixteen" 16
   "seventeen" 17 "eighteen" 18 "nineteen" 19
   "twenty" 20 "thirty" 30 "forty" 40 "fifty" 50
   "sixty" 60 "seventy" 70 "eighty" 80 "ninety" 90})

(defn- parse-words
  "ixt-sec:numwordsen. Nil when the text has no number word, so an outer
  tag that only wraps a nested fact is not read as zero."
  [text]
  (let [words (->> (-> (str/lower-case (str text))
                       (str/replace #"," "")
                       (str/replace #"-" " ")
                       (str/split #"\s+"))
                   (remove str/blank?)
                   (remove #{"and"}))]
    (when (seq words)
      (loop [words words acc 0 current 0]
        (if-let [w (first words)]
          (case w
            "hundred" (recur (rest words) acc (* (if (zero? current) 1 current) 100))
            "thousand" (recur (rest words) (+ acc (* (if (zero? current) 1 current) 1000)) 0)
            "million" (recur (rest words) (+ acc (* (if (zero? current) 1 current) 1000000)) 0)
            (if-let [n (get word-value w)]
              (recur (rest words) acc (+ current n))
              nil))
          (bigdec (+ acc current)))))))

(defn- comma-decimal?
  "True when the ixt format puts the decimal mark on the comma, as
  `ixt:num-comma-decimal` and `ixt:numspacecomma` do. A format's last
  mention of `dot` or `comma` is the decimal mark, because a group
  separator is named first (`num-dot-comma` is a comma decimal)."
  [fmt]
  (let [f (str/lower-case (str fmt))
        dot (str/last-index-of f "dot")
        comma (str/last-index-of f "comma")]
    (boolean (and comma (or (nil? dot) (> comma dot))))))

(defn- parse-decimal
  "Displayed number in an ixt numeric format.

  `ixt:num-dot-decimal` groups thousands with commas; a comma-decimal
  format, such as `ixt:num-comma-decimal`, `ixt:numspacecomma`, or
  `ixt:numdotcomma`, groups with dots. Spaces and non-breaking spaces are
  always grouping. Parentheses are a minus sign. Nil when the text is not
  a number, so a dash that `fixed-zero` did not claim is dropped."
  ([text] (parse-decimal text nil))
  ([text fmt]
   (let [s (-> (str text)
               (str/replace #"[\u00a0\u2007\u202f]" " ")
               str/trim)
         neg? (boolean (re-matches #"\(\s*.*\s*\)" s))
         s (if neg? (subs s 1 (dec (count s))) s)
         s (str/replace s #"\s" "")
         s (if (comma-decimal? fmt)
             (-> s (str/replace #"\." "") (str/replace "," "."))
             (str/replace s "," ""))]
     (when (re-matches #"-?\d+(?:\.\d+)?" s)
       (cond-> (bigdec s) neg? (.negate))))))

(defn- dash-text?
  "True when the displayed amount is only a dash or a minus sign, which a
  `dash` format uses for zero."
  [text]
  (boolean (re-matches #"[\s\u2010-\u2015\u2212-]*" (str text))))

(defn- format-kind
  [fmt]
  (let [f (str/lower-case (str fmt))]
    (cond
      (= f "ixt:fixed-zero") :zero
      (str/ends-with? f "numwordsen") :words
      (or (str/ends-with? f "zero-dash")
          (str/ends-with? f "zerodash")
          (str/ends-with? f "numdash")) :dash
      :else :number)))

(defn- long-attr
  [el k default]
  (let [s (.attr el k)]
    (if (str/blank? s)
      default
      (try (parse-long s)
           (catch Exception _ default)))))

(defn- apply-sign
  [^BigDecimal v sign]
  (if (= sign "-") (.negate v) v))

(defn- bd->num
  "Long when the scaled value is a whole number, otherwise a double."
  [^BigDecimal v]
  (try
    (.longValueExact v)
    (catch ArithmeticException _
      (double v))))

(defn fact-value
  "Scaled number for one ix:nonFraction, or nil when it is nil or not a
  number. `fixed-zero` is zero whatever the page shows, and a `dash`
  format is zero when the page shows only a dash. The sign attribute
  negates the displayed amount. `scale` is the power of ten applied after
  that, and may be negative."
  [el text]
  (let [fmt (.attr el "format")]
    (when-not (= "true" (.attr el "xsi:nil"))
      (when-let [base (case (format-kind fmt)
                        :zero (bigdec 0)
                        :words (parse-words text)
                        :dash (if (dash-text? text) (bigdec 0) (parse-decimal text fmt))
                        :number (parse-decimal text fmt))]
        (bd->num (.movePointRight (apply-sign base (.attr el "sign"))
                                  (int (long-attr el "scale" 0))))))))

;; Element lookup ignores the namespace prefix and case.
;;
;; The XBRL instance namespace may be bound to any prefix: SEC's extracted
;; instances use the default namespace (`<context>`), an inline document
;; writes `<xbrli:context>`, and ZENA's 2026 20-F/A declares `xmlns:i` and
;; writes `<i:context>` / `<i:unit>`. Matching only the `xbrli:` spelling or
;; the bare local name found no contexts there, so every one of that
;; filing's 963 facts was dropped: a fact whose context cannot be found is
;; never read. Only the local part of the tag is compared.
;;
;; The concept name keeps its own prefix. A filer that rebinds the us-gaap
;; prefix (rather than the instance prefix) would still need a namespace
;; map, which this reader does not build.

(defn- local-tag
  "Lowercased tag name without its namespace prefix."
  [^Element el]
  (let [n (str/lower-case (.tagName el))
        i (str/last-index-of n ":")]
    (if i (subs n (inc i)) n)))

(defn elements
  "Descendants of `el` whose local tag name is the local part of `tag`.
  `tag` may be written with any prefix, or none, and in any case."
  [^Element el tag]
  (let [s (str tag)
        i (str/last-index-of s ":")
        want (str/lower-case (if i (subs s (inc i)) s))]
    (filter #(= want (local-tag %)) (.getAllElements el))))

(defn- own-text
  "Text of `el`, without a nested fact, an ix:exclude, or a footnote."
  [el]
  (let [copy (.clone el)]
    (doseq [nested (elements copy "ix:nonfraction")]
      (when-not (identical? nested copy)
        (.remove nested)))
    (doseq [x (elements copy "ix:exclude")]
      (.remove x))
    (doseq [x (elements copy "ix:footnote")]
      (.remove x))
    (.text copy)))

(defn continued-text
  "Fact text, following an ix:continuation chain."
  [el by-id]
  (loop [id (.attr el "continuedat")
         parts [(own-text el)]
         seen #{}]
    (if (or (str/blank? id) (contains? seen id) (not (contains? by-id id)))
      (str/join " " parts)
      (let [next (get by-id id)]
        (recur (.attr next "continuedat")
               (conj parts (own-text next))
               (conj seen id))))))

(defn first-text
  [el tag]
  (when el
    (when-let [child (first (elements el tag))]
      (not-empty (str/trim (.text child))))))

(defn period
  [ctx]
  (if-let [instant (first-text ctx "xbrli:instant")]
    {:end instant}
    (when-let [end (first-text ctx "xbrli:enddate")]
      (cond-> {:end end}
        (first-text ctx "xbrli:startdate")
        (assoc :start (first-text ctx "xbrli:startdate"))))))

(defn read-continuations
  [doc]
  (into {}
        (for [el (elements doc "ix:continuation")
              :let [id (not-empty (.id el))]
              :when id]
          [id el])))

(defn split-qname
  [qname]
  (let [s (str/trim (str qname))
        i (str/last-index-of s ":")]
    (when (and i (pos? i) (< i (dec (count s))))
      [(keyword (subs s 0 i)) (keyword (subs s (inc i)))])))

(defn clean-text
  [s]
  (-> (str s)
      (str/replace #"\u00a0" " ")
      (str/replace #"\s+" " ")
      str/trim
      not-empty))

(defn attr
  "Attribute `k`, ignoring case. An instance keeps `contextRef`; HTML
  lowercases it to `contextref`."
  [el k]
  (let [want (str/lower-case (str k))]
    (some (fn [a]
            (when (= want (str/lower-case (.getKey a)))
              (not-empty (.getValue a))))
          (.attributes el))))

(defn instance-document?
  "True when `text` is an XBRL instance rather than a linkbase, a schema,
  or a plain HTML page.

  SEC's older EX-101 instances write the root as `<xbrli:xbrl>`, not
  `<xbrl>`. Parsing one of those as HTML lowercases every tag name, which
  turns `us-gaap:WeightedAverageNumberOfSharesOutstandingBasic` into a
  different concept and silently drops the share count."
  [text]
  (let [s (str text)]
    (boolean (re-find #"(?i)<(?:[a-z0-9_-]+:)?xbrl[\s>]" s))))

(defn parse-doc
  "HTML for a filing cover. An extracted instance (`<xbrl>`) is XML, so
  concept tags such as `ifrs-full:WeightedAverageShares` keep their case."
  [text]
  (if (instance-document? text)
    (Jsoup/parse (str text) "" (Parser/xmlParser))
    (Jsoup/parse (str text))))

(defn qname-pair
  "Taxonomy and concept keywords from a QName. A name with no prefix keeps
  the whole name as the concept and a nil taxonomy."
  [qname]
  (or (split-qname qname)
      (let [s (str/trim (str qname))]
        [nil (when-not (str/blank? s) (keyword s))])))

(defn- qname-keyword
  "One QName as a keyword. `us-gaap:Statement...` becomes
  `:us-gaap/Statement...`; a name with no prefix stays unprefixed."
  [qname]
  (let [s (str/trim (str qname))
        i (str/last-index-of s ":")]
    (cond
      (str/blank? s) nil
      (and i (pos? i) (< i (dec (count s))))
      (keyword (subs s 0 i) (subs s (inc i)))
      :else (keyword s))))

(defn context-dimension-rows
  "`:dimension` rows for one context. An explicit member splits axis and
  member into taxonomy/concept keywords. A typed member has no member
  QName, so its trimmed text goes in `:member-value`."
  [ctx context-id]
  (vec
   (keep (fn [mem]
           (when-let [ax (attr mem "dimension")]
             (let [[at ac] (qname-pair ax)]
               (if (= "typedmember" (local-tag mem))
                 {:context-id context-id
                  :axis-taxonomy at
                  :axis-concept ac
                  :member-taxonomy nil
                  :member-concept nil
                  :member-value (str/trim (.text mem))}
                 (let [[mt mc] (qname-pair (str/trim (.text mem)))]
                   {:context-id context-id
                    :axis-taxonomy at
                    :axis-concept ac
                    :member-taxonomy mt
                    :member-concept mc})))))
         (concat (elements ctx "xbrldi:explicitmember")
                 (elements ctx "xbrldi:typedmember")))))

(defn- context-dimensions
  "Axis to member for one context's explicit and typed members. An explicit
  member is a namespaced keyword; a typed member keeps its trimmed text as a
  string. A repeated axis keeps the last member."
  [ctx]
  (into (sorted-map)
        (keep (fn [mem]
                (when-let [ax (not-empty (attr mem "dimension"))]
                  (let [v (not-empty (str/trim (.text mem)))]
                    (when v
                      [(qname-keyword ax)
                       (if (= "typedmember" (local-tag mem))
                         v
                         (qname-keyword v))])))))
        (concat (elements ctx "xbrldi:explicitmember")
                (elements ctx "xbrldi:typedmember"))))

(defn context-row
  "One `:context` row: period, entity, and axis→member dimensions. Nil when
  the context has no usable period."
  [ctx context-id]
  (when-let [p (period ctx)]
    (let [entity (first (elements ctx "xbrli:entity"))
          identifier (when entity (first (elements entity "xbrli:identifier")))]
      (cond-> {:context-id context-id
               :period (if (:start p)
                         {:start (:start p) :end (:end p)}
                         {:instant (:end p)})
               :dimension (context-dimensions ctx)}
        entity (assoc :entity
                      {:scheme (when identifier (attr identifier "scheme"))
                       :identifier (first-text entity "xbrli:identifier")})))))

(defn ordered-contexts
  "`[id element]` pairs in first-seen order across `documents`. A repeated
  id keeps its first definition."
  [documents]
  (let [[out _]
        (reduce (fn [[out seen] doc]
                  (reduce (fn [[out seen] ctx]
                            (let [id (not-empty (or (attr ctx "id") (.id ctx)))]
                              (if (and id (not (contains? seen id)))
                                [(conj out [id ctx]) (conj seen id)]
                                [out seen])))
                          [out seen]
                          (elements doc "xbrli:context")))
                [[] #{}]
                documents)]
    out))

(defn fact-element?
  "True for an inline fact (`name` attribute) or an extracted-instance fact
  (a prefixed, non-structural element with a context reference)."
  [el]
  (or (some? (attr el "name"))
      (and (some? (attr el "contextref"))
           (let [tag (.tagName el)]
             (and (str/includes? tag ":")
                  (not (re-find #"(?i)^(xbrli|ix|xbrldi|link):" tag)))))))

(defn numeric-fact?
  "Inline nonFraction, or an extracted-instance fact carrying a unit reference."
  [el]
  (or (= "nonfraction" (local-tag el))
      (some? (attr el "unitref"))))

