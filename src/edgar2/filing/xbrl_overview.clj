(ns edgar2.filing.xbrl-overview
  "Flat facts/contexts/dimensions view over documents parsed by
  `edgar2.filing.xbrl`."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.filing.xbrl :as xbrl]))

(defn- overview-fact
  "One `:facts` row. A numeric fact is parsed to a number; a non-numeric
  fact keeps its trimmed text. `dims-by-context` maps a context id to its
  dimension rows without `:context-id`."
  [el dims-by-context continuations]
  (let [qname (or (xbrl/attr el "name") (.tagName el))
        [taxonomy concept] (xbrl/qname-pair qname)
        context-id (xbrl/attr el "contextref")
        text (xbrl/continued-text el continuations)
        numeric? (xbrl/numeric-fact? el)]
    {:taxonomy taxonomy
     :concept concept
     :fact-id (xbrl/attr el "id")
     :context-id context-id
     :dimensions (get dims-by-context context-id [])
     :value (if numeric? (xbrl/fact-value el text) (xbrl/clean-text text))
     :unit (xbrl/attr el "unitref")}))

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
  (let [documents (mapv #(xbrl/parse-doc (:text %)) docs)
        continuations (reduce merge {} (map xbrl/read-continuations documents))
        contexts (xbrl/ordered-contexts documents)
        context-rows (vec (keep (fn [[id ctx]] (xbrl/context-row ctx id)) contexts))
        dimension-rows (vec (mapcat (fn [[id ctx]] (xbrl/context-dimension-rows ctx id))
                                    contexts))
        dims-by-context (into {}
                              (map (fn [rows]
                                     [(-> rows first :context-id)
                                      (mapv #(dissoc % :context-id) rows)]))
                              (vals (group-by :context-id dimension-rows)))
        facts (vec (->> documents
                        (mapcat #(.getAllElements %))
                        (filter xbrl/fact-element?)
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
