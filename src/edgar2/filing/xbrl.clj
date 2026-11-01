(ns edgar2.filing.xbrl
  "One inline XBRL filing HTML, as a companyfacts map.

  The shape matches data.sec.gov companyfacts (see test/facts/SONY.json):
  `:cik`, `:entityName`, and `:facts` of taxonomy, concept, `:units`,
  then observations. The instance carries no concept label or description,
  so those keys are left off. `:fy`, `:fp`, and `:form` come from the DEI
  cover and are stamped on every fact, the way companyfacts stamps the
  filing's own fiscal focus onto comparative columns. `:accn` and `:filed`
  are not in the HTML; pass them in `opts` when they are known.

  Companyfacts drops a fact that sits on an axis. Those stay here, with
  `:dimensions` on the observation, so each class of stock keeps its own
  share count.

  Parsed with jsoup. A filing is HTML, and a strict XML read rejects the
  ones that are not well-formed."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str])
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

(defn- fact-value
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

(defn- elements
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

(defn- continued-text
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

(defn- first-text
  [el tag]
  (when el
    (when-let [child (first (elements el tag))]
      (not-empty (str/trim (.text child))))))

(defn- measure-name
  "Companyfacts unit from one measure. iso4217:USD is USD, xbrli:shares
  is shares, xbrli:pure is pure, and a custom measure keeps its local name."
  [container]
  (when-let [m (first-text container "xbrli:measure")]
    (cond
      (str/starts-with? m "iso4217:") (subs m (count "iso4217:"))
      (= m "xbrli:shares") "shares"
      (= m "xbrli:pure") "pure"
      :else (let [i (str/index-of m ":")]
              (if i (subs m (inc i)) m)))))

(defn- child-element
  "First descendant of `el` whose local tag name is `local`, ignoring case
  and prefix. An inline document writes `xbrli:unitNumerator`; an
  extracted instance uses the default namespace and writes
  `unitNumerator`."
  [^Element el local]
  (first (elements el local)))

(defn- unit-name
  [el]
  (if-let [div (child-element el "divide")]
    (let [num (measure-name (child-element div "unitnumerator"))
          den (measure-name (child-element div "unitdenominator"))]
      (when (and num den)
        (str num "/" den)))
    (measure-name el)))

(defn- dimensions
  "Axis to member for an explicit or typed dimension. Nil when the context
  has none."
  [ctx]
  (let [members (concat (elements ctx "xbrldi:explicitmember")
                        (elements ctx "xbrldi:typedmember"))]
    (when (seq members)
      (not-empty
       (into (sorted-map)
             (keep (fn [mem]
                     (let [dim (not-empty (.attr mem "dimension"))
                           val (not-empty (str/trim (.text mem)))]
                       (when (and dim val)
                         [dim val]))))
             members)))))

(defn- period
  [ctx]
  (if-let [instant (first-text ctx "xbrli:instant")]
    {:end instant}
    (when-let [end (first-text ctx "xbrli:enddate")]
      (cond-> {:end end}
        (first-text ctx "xbrli:startdate")
        (assoc :start (first-text ctx "xbrli:startdate"))))))

(defn- read-contexts
  [doc]
  (into {}
        (for [ctx (elements doc "xbrli:context")
              :let [id (not-empty (.attr ctx "id"))
                    p (when id (period ctx))
                    dims (when p (dimensions ctx))]
              :when p]
          [id (cond-> p
                dims (assoc :dimensions dims))])))

(defn- read-units
  [doc]
  (into {}
        (for [unit (elements doc "xbrli:unit")
              :let [id (not-empty (.attr unit "id"))
                    label (when id (unit-name unit))]
              :when label]
          [id label])))

(defn- read-continuations
  [doc]
  (into {}
        (for [el (elements doc "ix:continuation")
              :let [id (not-empty (.id el))]
              :when id]
          [id el])))

(defn- split-qname
  [qname]
  (let [s (str/trim (str qname))
        i (str/last-index-of s ":")]
    (when (and i (pos? i) (< i (dec (count s))))
      [(keyword (subs s 0 i)) (keyword (subs s (inc i)))])))

(defn- non-numeric
  [doc qname]
  (some (fn [el]
          (when (= qname (.attr el "name"))
            (not-empty (str/trim (.text el)))))
        (elements doc "ix:nonnumeric")))

(defn- parse-cik
  [s]
  (when-let [digits (some->> s (re-find #"\d+"))]
    (parse-long digits)))

(defn- parse-year
  [s]
  (when-let [y (some->> s (re-find #"\d{4}"))]
    (parse-long y)))

(def ^:private cover-qnames
  "Inline text the cover uses for each listed security."
  #{"dei:security12btitle"
    "dei:tradingsymbol"
    "dei:securityexchangename"})

(defn- clean-text
  [s]
  (-> (str s)
      (str/replace #"\u00a0" " ")
      (str/replace #"\s+" " ")
      str/trim
      not-empty))

(defn- header
  "Filing fields shared by every observation. `opts` supplies `:accn` and
  `:filed`, and overrides the DEI cover for `:form`, `:fy`, and `:fp`."
  [doc opts]
  {:cik (parse-cik (non-numeric doc "dei:EntityCentralIndexKey"))
   :entityName (non-numeric doc "dei:EntityRegistrantName")
   :accn (:accn opts)
   :filed (:filed opts)
   :form (or (:form opts) (non-numeric doc "dei:DocumentType"))
   :fy (or (:fy opts) (parse-year (non-numeric doc "dei:DocumentFiscalYearFocus")))
   :fp (or (:fp opts) (non-numeric doc "dei:DocumentFiscalPeriodFocus"))})

(defn- attr
  "Attribute `k`, ignoring case. An instance keeps `contextRef`; HTML
  lowercases it to `contextref`."
  [el k]
  (let [want (str/lower-case (str k))]
    (some (fn [a]
            (when (= want (str/lower-case (.getKey a)))
              (not-empty (.getValue a))))
          (.attributes el))))

(defn- fact-qname
  "Inline facts name the concept in the `name` attribute. An extracted
  instance names it with the element itself."
  [el]
  (or (attr el "name")
      (when (and (attr el "contextref") (attr el "unitref"))
        (.tagName el))))

(defn- cover-fact
  "One dei cover string, with the context's dimensions. Nil for any other
  non-numeric fact, or when the context is missing."
  [el contexts continuations]
  (let [qname (attr el "name")]
    (when (and qname (contains? cover-qnames (str/lower-case qname)))
      (when-let [ctx-id (attr el "contextref")]
        (when-let [ctx (get contexts ctx-id)]
          (when-let [val (clean-text (continued-text el continuations))]
            (cond-> {:qname qname
                     :context ctx-id
                     :val val}
              (:dimensions ctx) (assoc :dimensions (:dimensions ctx)))))))))

(defn- cover-facts
  "Security title, ticker, and exchange from the cover. A repeated tag of
  the same value is kept once."
  [docs contexts continuations]
  (->> docs
       (mapcat (fn [doc] (elements doc "ix:nonnumeric")))
       (keep #(cover-fact % contexts continuations))
       distinct
       vec))

(defn- observation
  [el contexts units continuations header]
  (let [[taxonomy concept] (split-qname (fact-qname el))
        ctx (get contexts (attr el "contextref"))
        unit (get units (attr el "unitref"))
        val (fact-value el (continued-text el continuations))]
    (when (and taxonomy concept ctx unit (some? val) (:end ctx))
      (cond-> {:taxonomy taxonomy
               :concept concept
               :unit unit
               :end (:end ctx)
               :val val}
        (:start ctx) (assoc :start (:start ctx))
        (:accn header) (assoc :accn (:accn header))
        (some? (:fy header)) (assoc :fy (:fy header))
        (:fp header) (assoc :fp (:fp header))
        (:form header) (assoc :form (:form header))
        (:filed header) (assoc :filed (:filed header))
        (seq (:dimensions ctx)) (assoc :dimensions (:dimensions ctx))))))

(defn- obs-sort-key
  [obs]
  [(str (:end obs))
   (str (:start obs))
   (str (:dimensions obs))
   (str (:val obs))])

(defn- unique-key
  [obs]
  [(:taxonomy obs) (:concept obs) (:unit obs)
   (:start obs) (:end obs) (:dimensions obs) (:val obs)])

(defn- unique-obs
  "Drop a repeated tag of the same fact. The cover often prints a number
  that the hidden section also tags."
  [observations]
  (second
   (reduce (fn [[seen out] obs]
             (let [k (unique-key obs)]
               (if (contains? seen k)
                 [seen out]
                 [(conj seen k) (conj out obs)])))
           [#{} []]
           observations)))

(defn- sorted-units
  [units]
  (into (sorted-map)
        (map (fn [[unit rows]]
               [unit (vec (sort-by obs-sort-key rows))]))
        units))

(defn- nest
  "taxonomy → concept → {:units {unit [observations]}}."
  [observations]
  (let [grouped (reduce (fn [m obs]
                          (update-in m [(:taxonomy obs) (:concept obs) (:unit obs)]
                                     (fnil conj [])
                                     (dissoc obs :taxonomy :concept :unit)))
                        {}
                        observations)]
    (into (sorted-map)
          (for [[taxonomy concepts] grouped]
            [taxonomy
             (into (sorted-map)
                   (for [[concept units] concepts]
                     [concept {:units (sorted-units units)}]))]))))

(defn- companyfacts
  "Companyfacts map from jsoup `docs`. The first document supplies DEI.
  Contexts, units, and continuations are shared, so a fact in a later
  document can use a context defined only in an earlier one."
  [docs opts]
  (let [header (header (first docs) opts)
        contexts (reduce merge {} (map read-contexts docs))
        units (reduce merge {} (map read-units docs))
        continuations (reduce merge {} (map read-continuations docs))
        facts (->> docs
                   (mapcat #(.getAllElements %))
                   (filter fact-qname)
                   (keep #(observation % contexts units continuations header))
                   unique-obs
                   nest)
        cover (cover-facts docs contexts continuations)]
    (-> (cond-> {}
          (:cik header) (assoc :cik (:cik header))
          (:entityName header) (assoc :entityName (:entityName header)))
        (assoc :facts facts)
        (cond-> (seq cover) (assoc :cover cover)))))

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

(defn- parse-doc
  "HTML for a filing cover. An extracted instance (`<xbrl>`) is XML, so
  concept tags such as `ifrs-full:WeightedAverageShares` keep their case."
  [text]
  (if (instance-document? text)
    (Jsoup/parse (str text) "" (Parser/xmlParser))
    (Jsoup/parse (str text))))

(defn parse
  "Companyfacts-shaped map from inline XBRL HTML `text`.

  `opts` may include `:accn` and `:filed`. `:form`, `:fy`, and `:fp`
  override the DEI cover values when passed.

  Units are string keys (`\"USD\"`, `\"shares\"`, `\"USD/shares\"`) so a
  per-share unit is not split into a namespaced keyword.

  When the cover tags a listed security, `:cover` holds
  `dei:Security12bTitle`, `dei:TradingSymbol`, and
  `dei:SecurityExchangeName` with the context dimensions."
  ([text] (parse text {}))
  ([text opts]
   (companyfacts [(parse-doc text)] opts)))

(defn parse-all
  "Companyfacts map from several inline XBRL HTML documents.

  The first document is the cover and supplies DEI. Contexts, units, and
  continuations are merged before facts are read. An extracted instance
  contributes its own facts as well as the contexts the cover left out."
  ([texts] (parse-all texts {}))
  ([texts opts]
   (companyfacts (mapv parse-doc texts) opts)))

(defn- qname-pair
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

(defn- context-dimension-rows
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

(defn- context-row
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

(defn- ordered-contexts
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

(defn- fact-element?
  "True for an inline fact (`name` attribute) or an extracted-instance fact
  (a prefixed, non-structural element with a context reference)."
  [el]
  (or (some? (attr el "name"))
      (and (some? (attr el "contextref"))
           (let [tag (.tagName el)]
             (and (str/includes? tag ":")
                  (not (re-find #"(?i)^(xbrli|ix|xbrldi|link):" tag)))))))

(defn- numeric-fact?
  "Inline nonFraction, or an extracted-instance fact carrying a unit reference."
  [el]
  (or (= "nonfraction" (local-tag el))
      (some? (attr el "unitref"))))

(defn- overview-fact
  "One `:facts` row. A numeric fact is parsed to a number; a non-numeric
  fact keeps its trimmed text. `dims-by-context` maps a context id to its
  dimension rows without `:context-id`."
  [el dims-by-context continuations]
  (let [qname (or (attr el "name") (.tagName el))
        [taxonomy concept] (qname-pair qname)
        context-id (attr el "contextref")
        text (continued-text el continuations)
        numeric? (numeric-fact? el)]
    {:taxonomy taxonomy
     :concept concept
     :fact-id (attr el "id")
     :context-id context-id
     :dimensions (get dims-by-context context-id [])
     :value (if numeric? (fact-value el text) (clean-text text))
     :unit (attr el "unitref")}))

(defn overview
  "Flat facts/contexts/dimensions view of filing `docs`.

  `docs` is the vector from `edgar2.filing.xbrl-loader/load-documents`, each
  entry `{:text ... :where ...}`. Returns:

  - `:facts` — every fact occurrence, numeric and non-numeric, with
    `:taxonomy`, `:concept`, `:fact-id`, `:context-id`, `:dimensions`,
    `:value`, and `:unit` (the raw instance unit id, nil for non-numeric).
  - `:context` — one row per context: `:context-id`, `:period`, `:entity`
    (when the context has one), and `:dimension`, an axis→member map.
  - `:dimension` — one row per axis/member with `:context-id`,
    `:axis-taxonomy`, `:axis-concept`, `:member-taxonomy`, and
    `:member-concept`. A typed member also has `:member-value`.

  Unlike `parse`, a context is kept even if no fact uses it, repeated facts
  are kept, and a fact whose context id does not resolve keeps its raw
  `:context-id` with no dimensions."
  [docs]
  (let [documents (mapv #(parse-doc (:text %)) docs)
        continuations (reduce merge {} (map read-continuations documents))
        contexts (ordered-contexts documents)
        context-rows (vec (keep (fn [[id ctx]] (context-row ctx id)) contexts))
        dimension-rows (vec (mapcat (fn [[id ctx]] (context-dimension-rows ctx id))
                                    contexts))
        dims-by-context (into {}
                              (map (fn [rows]
                                     [(-> rows first :context-id)
                                      (mapv #(dissoc % :context-id) rows)]))
                              (vals (group-by :context-id dimension-rows)))
        facts (vec (->> documents
                        (mapcat #(.getAllElements %))
                        (filter fact-element?)
                        (map #(overview-fact % dims-by-context continuations))))]
    {:facts facts
     :context context-rows
     :dimension dimension-rows}))

(defn printable-period
  "A duration as `start-end`, an instant as its date. Nil when absent.

  A context carries either a `:start`/`:end` pair or a single `:instant`,
  so an absent (or empty) period prints nothing instead of an empty range."
  [period]
  (when period
    (if (and (:start period) (:end period))
      (str (:start period) "-" (:end period))
      (:instant period))))

(def ^:private max-printable-dimension 20)

(defn printable-dimension
  "Dimension member values joined with `\", \"`, capped at 20 characters.
  Axis keys are not included. Nil when the map is nil or empty.

  An explicit member is a keyword and keeps the leading colon `str` adds; a
  typed member is already plain text. Contexts without axis/member pairs
  have an empty map, which reads as a blank cell rather than as an empty
  string."
  [dimension]
  (when (seq dimension)
    (let [s (str/join ", " (map str (vals dimension)))]
      (subs s 0 (min max-printable-dimension (count s))))))

(defn- printable-context
  [row]
  (let [{:keys [period dimension value]} row
        identifier (or (:identifier row) (get-in row [:entity :identifier]))]
    (cond-> row
      period (assoc :period (printable-period period))
      identifier (assoc :identifier identifier)
      (seq dimension) (assoc :dimension (printable-dimension dimension))
      value (assoc :value (if (string? value)
                            (subs value 0 (min 10 (count value)))
                            value)))))

(defn facts-with-concepts
  "Facts joined to their context's period, entity identifier, and
  dimension by :context-id. A fact whose context does not resolve keeps
  its raw :context-id and gets nil joined fields."
  [data]
  (let [contexts (into {} (map (juxt :context-id identity)) (:context data))]
    (mapv (fn [fact]
            (let [ctx (get contexts (:context-id fact))]
              (assoc fact
                     :period (:period ctx)
                     :identifier (get-in ctx [:entity :identifier])
                     :dimension (:dimension ctx))))
          (:facts data))))

(defn print-concepts
  [summary]
  (->> (facts-with-concepts summary)
       (map printable-context)
       (map #(select-keys % [:taxonomy :concept :period :value :entity :dimension]))
       (pprint/print-table)))

(defn print-context
  "Print context rows as a table: a duration as a start-end range, the
  entity identifier, and the dimension members without their axes."
  [contexts]
  (pprint/print-table (map printable-context contexts)))

(defn- statement-exhibit?
  "True for the names an audited-statement exhibit takes beside the cover:
  `_d2.htm`, `ex99d2.htm`, `ex99-2.htm`, `ex-99.2.htm`, `exhibit99-2.htm`,
  and `exhibit992.htm`."
  [name]
  (let [n (str/lower-case (str name))]
    (boolean
     (or (re-find #"[-_]d2\.html?$" n)
         (re-find #"ex(?:hibit)?[-_]?99[-_.]?2\.html?$" n)
         (re-find #"ex99d2\.html?$" n)))))

(defn statement-exhibit-name
  "Filename of the audited-statement exhibit linked from cover HTML.

  Matches a same-filing relative name: `_d2.htm`, `ex99d2.htm`, or an
  `ex99-2` / `exhibit99-2` variant. An absolute archive URL is a citation
  of another filing, not this cover's exhibit. Nil when the cover has no
  such link."
  [html]
  (some (fn [[_ href]]
          (when-not (str/includes? (str href) "/")
            (let [name (str href)]
              (when (statement-exhibit? name)
                name))))
        (re-seq #"(?i)href\s*=\s*\"([^\"]+)\"" (str html))))
