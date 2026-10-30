(ns demo.concept-coverage
  "Study: how many listed stocks miss :revenue / :net-income concepts,
  and which alternate XBRL tags they actually file."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.financials.concepts :as concepts]
            [edgar2.download.facts :as dl]
            [edgar2.download.facts-universe :as universe]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.util :as util]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def revenue-tags (:data (concepts/get-concept-id :revenue)))
(def net-income-tags (:data (concepts/get-concept-id :net-income)))

(defn load-stats []
  (-> "data/stats.edn" slurp edn/read-string))

(defn load-universe []
  (let [m (-> "data/universe-facts.edn" slurp edn/read-string)]
    (or (:rows m) (:data m) (when (vector? m) m) [])))

(defn has-calc?
  "True when calc-concept yields a numeric value on the latest annual row."
  [facts concept-id]
  (let [fields (:data (concepts/get-concept-id concept-id))
        table (facts-ds/report-table facts :y fields)
        vals (facts-ds/calc-concept table concept-id)]
    (boolean (some number? vals))))

(defn concept-hits
  "Set of local concept names present (any unit, any form) in companyfacts map."
  [facts-map taxonomy]
  (->> (get facts-map taxonomy)
       keys
       (map name)
       set))

(defn revenue-like-names
  "Heuristic: concept names that look like top-line revenue."
  [names]
  (->> names
       (filter #(re-find #"(?i)revenue|sales|premium|interest.*income|fee.*income|turnover"
                         %))
       sort
       vec))

(defn income-like-names
  "Heuristic: concept names that look like bottom-line earnings."
  [names]
  (->> names
       (filter #(re-find #"(?i)netincome|profitloss|incomeloss|comprehensiveincome"
                         %))
       sort
       vec))

(defn known-tag-names [tag-pairs]
  (set (map (comp name second) tag-pairs)))

(defn analyze-facts-json
  "For one companyfacts JSON body: which of our tags hit, plus near-miss names."
  [data]
  (let [facts (:facts data)
        us (concept-hits facts :us-gaap)
        ifrs (concept-hits facts :ifrs-full)
        all (into us ifrs)
        rev-known (known-tag-names revenue-tags)
        ni-known (known-tag-names net-income-tags)
        rev-hit (filterv rev-known all)
        ni-hit (filterv ni-known all)]
    {:cik (universe/pad-cik (:cik data))
     :entityName (:entityName data)
     :standards (universe/reporting-standard facts)
     :rev-hit rev-hit
     :ni-hit ni-hit
     :has-rev (boolean (seq rev-hit))
     :has-ni (boolean (seq ni-hit))
     :rev-near (vec (remove rev-known (revenue-like-names all)))
     :ni-near (vec (remove ni-known (income-like-names all)))}))

(defn read-zip-json
  [zf entry-name]
  (let [entry (.getEntry zf entry-name)]
    (when entry
      (with-open [in (.getInputStream zf entry)]
        (json/read-value in json/keyword-keys-object-mapper)))))

(defn cik-entry-name
  [cik]
  (str "CIK" (universe/pad-cik cik) ".json"))

(defn study-missing-from-stats!
  "Re-parse companyfacts for tickers that lack aligned :revenue/:net-profit
  in stats.edn. Reports raw tag presence vs near-miss alternate tags."
  ([]
   (study-missing-from-stats! {}))
  ([{:keys [limit]}]
   (util/ensure-identity!)
   (let [stats (load-stats)
         missing (cond->> (filterv #(not (number? (:revenue %))) stats)
                   limit (take (long limit)))
         missing (vec missing)
         zip-path (dl/ensure-zip! {})]
     (println (format "stats total=%d  aligned-revenue/net-profit missing=%d  studying=%d"
                      (count stats) (count (filter #(not (number? (:revenue %))) stats))
                      (count missing)))
     (with-open [zf (ZipFile. (str zip-path))]
       (let [rows (mapv (fn [{:keys [cik ticker exchange entityName] :as row}]
                          (let [data (read-zip-json zf (cik-entry-name cik))
                                a (when data (analyze-facts-json data))]
                            (merge {:ticker ticker :exchange exchange :cik cik
                                    :entityName entityName
                                    :annual-count (:annual-count row)
                                    :sic (:sic row)}
                                   (or a {:error "no-json"}))))
                        missing)
             n (count rows)
             has-both (count (filter #(and (:has-rev %) (:has-ni %)) rows))
             has-rev (count (filter :has-rev rows))
             has-ni (count (filter :has-ni rows))
             neither (count (filter #(and (not (:has-rev %)) (not (:has-ni %))) rows))
             rev-only (count (filter #(and (:has-rev %) (not (:has-ni %))) rows))
             ni-only (count (filter #(and (not (:has-rev %)) (:has-ni %)) rows))
             by-std (frequencies (map :standards rows))
             ;; near-miss frequency among those missing the concept
             miss-rev (filter #(not (:has-rev %)) rows)
             miss-ni (filter #(not (:has-ni %)) rows)
             near-rev-freq (->> miss-rev
                                (mapcat :rev-near)
                                frequencies
                                (sort-by val >)
                                (take 25))
             near-ni-freq (->> miss-ni
                               (mapcat :ni-near)
                               frequencies
                               (sort-by val >)
                               (take 25))]
         (println)
         (println "=== Among stats-aligned misses: raw companyfacts tag presence ===")
         (println (format "  has both rev+ni tags: %d" has-both))
         (println (format "  has rev tags: %d  has ni tags: %d" has-rev has-ni))
         (println (format "  rev only: %d  ni only: %d  neither: %d"
                          rev-only ni-only neither))
         (println "  reporting-standard:" (sort-by val > by-std))
         (println)
         (println "=== Top alternate revenue-like tags (among raw-rev misses) ===")
         (pprint/pprint near-rev-freq)
         (println)
         (println "=== Top alternate net-income-like tags (among raw-ni misses) ===")
         (pprint/pprint near-ni-freq)
         (println)
         (println "=== Sample: has tags but no aligned margin (pipeline gap) ===")
         (pprint/print-table
          [:ticker :exchange :standards :rev-hit :ni-hit :annual-count]
          (->> rows
               (filter #(and (:has-rev %) (:has-ni %)))
               (take 15)
               (map #(update % :rev-hit (fn [v] (str/join "," (take 3 v)))))
               (map #(update % :ni-hit (fn [v] (str/join "," (take 3 v)))))))
         (println)
         (println "=== Sample: missing revenue tags (near alternatives) ===")
         (pprint/print-table
          [:ticker :exchange :standards :ni-hit :rev-near]
          (->> miss-rev
               (take 20)
               (map #(-> %
                         (update :ni-hit (fn [v] (str/join "," (take 2 v))))
                         (update :rev-near (fn [v] (str/join "," (take 4 v))))))))
         (println)
         (println "=== Sample: missing net-income tags (near alternatives) ===")
         (pprint/print-table
          [:ticker :exchange :standards :rev-hit :ni-near]
          (->> miss-ni
               (take 20)
               (map #(-> %
                         (update :rev-hit (fn [v] (str/join "," (take 2 v))))
                         (update :ni-near (fn [v] (str/join "," (take 4 v))))))))
         {:n n
          :has-both has-both
          :has-rev has-rev
          :has-ni has-ni
          :neither neither
          :rev-only rev-only
          :ni-only ni-only
          :near-rev near-rev-freq
          :near-ni near-ni-freq
          :rows rows})))))

(defn study-universe-calc!
  "Walk universe-facts with e/facts + calc-concept (slower; optional limit)."
  ([]
   (study-universe-calc! {:limit 200}))
  ([{:keys [limit]}]
   (util/ensure-identity!)
   (let [u (vec (cond->> (load-universe)
                  limit (take (long limit))))
         results
         (mapv (fn [{:keys [ticker cik] :as row}]
                 (try
                   (let [facts (e/facts (or ticker cik))
                         rev? (has-calc? facts :revenue)
                         ni? (has-calc? facts :net-income)]
                     {:ticker ticker :cik cik
                      :rev rev? :ni ni?
                      :u-rev (number? (:revenue row))})
                   (catch Exception ex
                     {:ticker ticker :cik cik :error (.getMessage ex)})))
               u)
         ok (remove :error results)]
     (println (format "calc-concept on %d (errors %d)"
                      (count ok) (count (filter :error results))))
     (println (format "  :revenue  miss %d"
                      (count (filter #(not (:rev %)) ok))))
     (println (format "  :net-income miss %d"
                      (count (filter #(not (:ni %)) ok))))
     (println "sample rev-miss" (mapv :ticker (take 20 (filter #(not (:rev %)) ok))))
     (println "sample ni-miss" (mapv :ticker (take 20 (filter #(not (:ni %)) ok))))
     results)))

(defn -main
  [& _]
  (study-missing-from-stats!)
  ;
  )
