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
  (:require [clojure.string :as str])
  (:import [java.math BigDecimal]
           [org.jsoup Jsoup]))

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
  "ixt-sec:numwordsen. Nil when a word is not a number."
  [text]
  (let [words (->> (-> (str/lower-case (str text))
                       (str/replace #"," "")
                       (str/replace #"-" " ")
                       (str/split #"\s+"))
                   (remove str/blank?)
                   (remove #{"and"}))]
    (loop [words words acc 0 current 0]
      (if-let [w (first words)]
        (case w
          "hundred" (recur (rest words) acc (* (if (zero? current) 1 current) 100))
          "thousand" (recur (rest words) (+ acc (* (if (zero? current) 1 current) 1000)) 0)
          "million" (recur (rest words) (+ acc (* (if (zero? current) 1 current) 1000000)) 0)
          (if-let [n (get word-value w)]
            (recur (rest words) acc (+ current n))
            nil))
        (bigdec (+ acc current))))))

(defn- parse-decimal
  "Displayed number. Commas and spaces are grouping. Parentheses are a
  minus sign."
  [text]
  (let [s (-> (str text)
              (str/replace #"\u00a0" "")
              (str/replace #"[\s,]" ""))]
    (cond
      (re-matches #"-?\d+(?:\.\d+)?" s) (bigdec s)
      (re-matches #"\(\d+(?:\.\d+)?\)" s)
      (.negate (bigdec (subs s 1 (dec (count s)))))
      :else nil)))

(defn- format-kind
  [fmt]
  (cond
    (or (= fmt "ixt:fixed-zero")
        (str/ends-with? (str fmt) ":zero-dash")) :zero
    (str/ends-with? (str fmt) "numwordsen") :words
    :else :number))

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
  number. `fixed-zero` is a dash on the page and means zero. The sign
  attribute negates the displayed amount. `scale` is the power of ten
  applied after that."
  [el text]
  (when-not (= "true" (.attr el "xsi:nil"))
    (when-let [base (case (format-kind (.attr el "format"))
                      :zero (bigdec 0)
                      :words (parse-words text)
                      :number (parse-decimal text))]
      (bd->num (.movePointRight (apply-sign base (.attr el "sign"))
                                (int (long-attr el "scale" 0)))))))

(defn- own-text
  "Text of `el`, without a nested fact, an ix:exclude, or a footnote."
  [el]
  (let [copy (.clone el)]
    (doseq [nested (.getElementsByTag copy "ix:nonfraction")]
      (when-not (identical? nested copy)
        (.remove nested)))
    (doseq [tag ["ix:exclude" "ix:footnote"]]
      (doseq [x (.getElementsByTag copy tag)]
        (.remove x)))
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
    (when-let [child (first (.getElementsByTag el tag))]
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

(defn- unit-name
  [el]
  (if-let [div (first (.getElementsByTag el "xbrli:divide"))]
    (let [num (measure-name (first (.getElementsByTag div "xbrli:unitnumerator")))
          den (measure-name (first (.getElementsByTag div "xbrli:unitdenominator")))]
      (when (and num den)
        (str num "/" den)))
    (measure-name el)))

(defn- dimensions
  "Axis to member for an explicit or typed dimension. Nil when the context
  has none."
  [ctx]
  (let [members (concat (.getElementsByTag ctx "xbrldi:explicitmember")
                        (.getElementsByTag ctx "xbrldi:typedmember"))]
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
        (for [ctx (.getElementsByTag doc "xbrli:context")
              :let [id (not-empty (.attr ctx "id"))
                    p (when id (period ctx))
                    dims (when p (dimensions ctx))]
              :when p]
          [id (cond-> p
                dims (assoc :dimensions dims))])))

(defn- read-units
  [doc]
  (into {}
        (for [unit (.getElementsByTag doc "xbrli:unit")
              :let [id (not-empty (.attr unit "id"))
                    label (when id (unit-name unit))]
              :when label]
          [id label])))

(defn- read-continuations
  [doc]
  (into {}
        (for [el (.getElementsByTag doc "ix:continuation")
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
        (.getElementsByTag doc "ix:nonnumeric")))

(defn- parse-cik
  [s]
  (when-let [digits (some->> s (re-find #"\d+"))]
    (parse-long digits)))

(defn- parse-year
  [s]
  (when-let [y (some->> s (re-find #"\d{4}"))]
    (parse-long y)))

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

(defn- observation
  [el contexts units continuations header]
  (let [[taxonomy concept] (split-qname (.attr el "name"))
        ctx (get contexts (.attr el "contextref"))
        unit (get units (.attr el "unitref"))
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
                   (mapcat #(.getElementsByTag % "ix:nonfraction"))
                   (keep #(observation % contexts units continuations header))
                   unique-obs
                   nest)]
    (-> (cond-> {}
          (:cik header) (assoc :cik (:cik header))
          (:entityName header) (assoc :entityName (:entityName header)))
        (assoc :facts facts))))

(defn parse
  "Companyfacts-shaped map from inline XBRL HTML `text`.

  `opts` may include `:accn` and `:filed`. `:form`, `:fy`, and `:fp`
  override the DEI cover values when passed.

  Units are string keys (`\"USD\"`, `\"shares\"`, `\"USD/shares\"`) so a
  per-share unit is not split into a namespaced keyword."
  ([text] (parse text {}))
  ([text opts]
   (companyfacts [(Jsoup/parse (str text))] opts)))

(defn parse-all
  "Companyfacts map from several inline XBRL HTML documents.

  The first document is the cover and supplies DEI. Contexts, units, and
  continuations are merged before facts are read."
  ([texts] (parse-all texts {}))
  ([texts opts]
   (companyfacts (mapv #(Jsoup/parse (str %)) texts) opts)))

(defn statement-exhibit-name
  "Filename of the audited-statement exhibit linked from cover HTML.

  Matches `_d2.htm` and `ex99d2.htm`. Nil when the cover has no such link."
  [html]
  (some (fn [[_ href]]
          (when-let [[_ name] (re-find #"(?i)([^/\"?#]+(?:_d2|ex99d2)\.html?)$"
                                       (str href))]
            name))
        (re-seq #"(?i)href\s*=\s*\"([^\"]+)\"" (str html))))
