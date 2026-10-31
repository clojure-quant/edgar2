(ns demo.xbrl
  "Share counts from a companyfacts file, or from the latest annual filing.

  clj -X:demo-xbrl
  clj -X:demo-xbrl :ticker '\"SONY\"'
  clj -X:demo-xbrl :file '\"test/facts/COKE.json\"'"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.db.universe :as universe]
            [edgar2.download.dataset :as dataset]
            [edgar2.filing.xbrl :as xbrl]
            [edgar2.util :as util]
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

(def ^:private class-axis
  :us-gaap:StatementClassOfStockAxis)

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

(defn- class-member
  "Value of the class-of-stock axis, or nil when the row has no class."
  [row]
  (map-get (:dimensions row) class-axis))

(defn- class-label
  "\"Class B\" for a ClassB member. No axis, or any other member, is \"Common\"."
  [member]
  (let [local (let [s (str member)
                    i (str/last-index-of s ":")]
                (if i (subs s (inc i)) s))]
    (if-let [[_ letter] (re-find #"Class([A-Z])Member$" local)]
      (str "Class " letter)
      "Common")))

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
  [concept]
  (group-by #(class-label (class-member %)) (share-rows concept)))

(defn share-summary
  "Latest weighted-average and shares outstanding by common-stock class.

  `facts` is a companyfacts map. A row with no
  `:us-gaap:StatementClassOfStockAxis` is \"Common\". A ClassA or ClassB
  member on that axis is \"Class A\" or \"Class B\"; any other member is
  \"Common\". Each figure is the observation with the greatest `:end`."
  [facts]
  (let [outstanding (rows-by-class
                     (get-in facts [:facts :dei :EntityCommonStockSharesOutstanding]))
        weighted (rows-by-class
                  (get-in facts [:facts :us-gaap :WeightedAverageNumberOfSharesOutstandingBasic]))
        classes (->> (concat (keys outstanding) (keys weighted))
                     distinct
                     (sort-by #(if (= % "Common") " " %)))]
    (into {}
          (map (fn [class]
                 [class {:weighted (latest-val (get weighted class))
                         :outstanding (latest-val (get outstanding class))}]))
          classes)))

(defn- ticker-name
  [ticker]
  (when (some? ticker)
    (let [s (str/upper-case
             (cond
               (string? ticker) ticker
               (keyword? ticker) (name ticker)
               :else (str ticker)))]
      (when-not (str/blank? s) s))))

(defn share-summary-cik
  "Latest annual filing for `cik`, parsed with `edgar2.filing.xbrl/parse`.

  The form is 10-K, or 20-F / 40-F when that is the annual report.
  Returns `{:cik cik :shares <share-summary>}`."
  [cik]
  (util/ensure-identity!)
  (let [form (ufiling/annual-form cik)
        filing (e/filing cik :form form)]
    (when-not filing
      (throw (ex-info "no annual filing" {:cik cik :form form})))
    (let [html (e/html filing)
          parsed (xbrl/parse html {:accn (:accessionNumber filing)
                                   :filed (:filingDate filing)
                                   :form (:form filing)})]
      {:cik cik
       :shares (share-summary parsed)})))

(defn share-summary-ticker
  "CIK for `ticker`, then `share-summary-cik`, with `:ticker` added."
  [ticker]
  (util/ensure-identity!)
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
  (util/ensure-identity!)
  (e/enable-disk-cache! :dir "data/edgar-cache")
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


; dei EntittyCommonStockSharesOutstanding
; from companyfacts (test/facts/COKE.json)
;{:filed "2014-03-14", :val 7141447, :fy 2013, :form "10-K", :accn "0001193125-14-100068", :end "2014-02-28",  :fp "FY"}
;{:filed "2014-05-09", :frame "CY2014Q1I", :val 7141447, :fy 2014, :form "10-Q", :accn "0001193125-14-192369", :end "2014-04-30", :fp "Q1"}
;{:filed "2014-08-08", :frame "CY2014Q2I", :val 7141447, :fy 2014, :form "10-Q", :accn "0001193125-14-303056", :end "2014-07-31", :fp "Q2"}
;{:filed "2014-11-07", :frame "CY2014Q3I", :val 7141447, :fy 2014, :form "10-Q", :accn "0001193125-14-403014", :end "2014-10-31", :fp "Q3"}
;{:filed "2015-03-13", :frame "CY2015Q1I", :val 7141447, :fy 2014, :form "10-K", :accn "0001193125-15-091165", :end "2015-02-27", :fp "FY"}
;{:filed "2016-03-18", :frame "CY2016Q1I", :val 7141447, :fy 2015, :form "10-K", :accn "0001564590-16-015048", :end "2016-03-04", :fp "FY"}

{:val 56517334,:fy 2025, :form "10-K", :end "2026-01-30",:fp "FY" 
 :dimensions {:us-gaap:StatementClassOfStockAxis "coke:CommonClassUndefinedMember"}}

{:val 10046960,:fy 2025,:form "10-K",:end "2026-01-30",  :fp "FY"
 :dimensions {:us-gaap:StatementClassOfStockAxis "us-gaap:CommonClassBMember"}}

; us-gaap WeightedAverageNumberOfSharesOutstandingBasic
{:start "2023-01-01", :val 10047000,:fy 2025, :form "10-K",:end "2023-12-31", :fp "FY"
 :dimensions {:us-gaap:StatementClassOfStockAxis "us-gaap:CommonClassBMember"}}

{:start "2024-01-01", :val 10047000,:fy 2025, :form "10-K",:end "2024-12-31", :fp "FY"
 :dimensions {:us-gaap:StatementClassOfStockAxis "us-gaap:CommonClassBMember"}}

{:start "2023-01-01", :val 83690000,:fy 2025, :form "10-K",:end "2023-12-31", :fp "FY"
 :dimensions {:us-gaap:StatementClassOfStockAxis "coke:CommonClassUndefinedMember"},
 }
  
  {:start "2024-01-01", :val 80348000,:fy 2025, :form "10-K",:end "2024-12-31", :fp "FY"
   :dimensions {:us-gaap:StatementClassOfStockAxis "coke:CommonClassUndefinedMember"},
   }
  
  {:start "2025-01-01", :val 73658000,:fy 2025, :form "10-K",:end "2025-12-31", :fp "FY"
   :dimensions {:us-gaap:StatementClassOfStockAxis "coke:CommonClassUndefinedMember"},
   }
  {:start "2025-01-01", :val 10047000,:fy 2025, :form "10-K",:end "2025-12-31", :fp "FY"
   :dimensions {:us-gaap:StatementClassOfStockAxis "us-gaap:CommonClassBMember"},
   }


;<xbrli:context id="c-12">
;  <xbrli:entity>
;    <xbrli:identifier scheme="http://www.sec.gov/CIK">0000317540</xbrli:identifier>
;    <xbrli:segment>
;      <xbrldi:explicitmember dimension="us-gaap:StatementClassOfStockAxis">us-gaap:CommonClassBMember</xbrldi:explicitmember>
;    </xbrli:segment>
;  </xbrli:entity>
;  <xbrli:period>
;    <xbrli:startdate>2023-01-01</xbrli:startdate>
;    <xbrli:enddate>2023-12-31</xbrli:enddate>
;   </xbrli:period>
; </xbrli:context>
