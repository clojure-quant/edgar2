(ns edgar2.filing.xbrl-facts
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

  The low-level document reader lives in `edgar2.filing.xbrl`."
  (:require [clojure.string :as str]
            [edgar2.filing.xbrl :as xbrl]))

(defn- measure-name
  "Companyfacts unit from one measure. iso4217:USD is USD, xbrli:shares
  is shares, xbrli:pure is pure, and a custom measure keeps its local name."
  [container]
  (when-let [m (xbrl/first-text container "xbrli:measure")]
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
  [el local]
  (first (xbrl/elements el local)))

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
  (let [members (concat (xbrl/elements ctx "xbrldi:explicitmember")
                        (xbrl/elements ctx "xbrldi:typedmember"))]
    (when (seq members)
      (not-empty
       (into (sorted-map)
             (keep (fn [mem]
                     (let [dim (not-empty (.attr mem "dimension"))
                           val (not-empty (str/trim (.text mem)))]
                       (when (and dim val)
                         [dim val]))))
             members)))))

(defn- read-contexts
  [doc]
  (into {}
        (for [ctx (xbrl/elements doc "xbrli:context")
              :let [id (not-empty (.attr ctx "id"))
                    p (when id (xbrl/period ctx))
                    dims (when p (dimensions ctx))]
              :when p]
          [id (cond-> p
                dims (assoc :dimensions dims))])))

(defn- read-units
  [doc]
  (into {}
        (for [unit (xbrl/elements doc "xbrli:unit")
              :let [id (not-empty (.attr unit "id"))
                    label (when id (unit-name unit))]
              :when label]
          [id label])))

(defn- non-numeric
  [doc qname]
  (some (fn [el]
          (when (= qname (.attr el "name"))
            (not-empty (str/trim (.text el)))))
        (xbrl/elements doc "ix:nonnumeric")))

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

(defn- fact-qname
  "Inline facts name the concept in the `name` attribute. An extracted
  instance names it with the element itself."
  [el]
  (or (xbrl/attr el "name")
      (when (and (xbrl/attr el "contextref") (xbrl/attr el "unitref"))
        (.tagName el))))

(defn- cover-fact
  "One dei cover string, with the context's dimensions. Nil for any other
  non-numeric fact, or when the context is missing."
  [el contexts continuations]
  (let [qname (xbrl/attr el "name")]
    (when (and qname (contains? cover-qnames (str/lower-case qname)))
      (when-let [ctx-id (xbrl/attr el "contextref")]
        (when-let [ctx (get contexts ctx-id)]
          (when-let [val (xbrl/clean-text (xbrl/continued-text el continuations))]
            (cond-> {:qname qname
                     :context ctx-id
                     :val val}
              (:dimensions ctx) (assoc :dimensions (:dimensions ctx)))))))))

(defn- cover-facts
  "Security title, ticker, and exchange from the cover. A repeated tag of
  the same value is kept once."
  [docs contexts continuations]
  (->> docs
       (mapcat (fn [doc] (xbrl/elements doc "ix:nonnumeric")))
       (keep #(cover-fact % contexts continuations))
       distinct
       vec))

(defn- observation
  [el contexts units continuations header]
  (let [[taxonomy concept] (xbrl/split-qname (fact-qname el))
        ctx (get contexts (xbrl/attr el "contextref"))
        unit (get units (xbrl/attr el "unitref"))
        val (xbrl/fact-value el (xbrl/continued-text el continuations))]
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
        continuations (reduce merge {} (map xbrl/read-continuations docs))
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
   (companyfacts [(xbrl/parse-doc text)] opts)))

(defn parse-all
  "Companyfacts map from several inline XBRL HTML documents.

  The first document is the cover and supplies DEI. Contexts, units, and
  continuations are merged before facts are read. An extracted instance
  contributes its own facts as well as the contexts the cover left out."
  ([texts] (parse-all texts {}))
  ([texts opts]
   (companyfacts (mapv xbrl/parse-doc texts) opts)))
