(ns edgar2.financials.facts-ds-converter
  "Freeze annual and quarterly companyfacts reports to data/facts.nippy.gz.

  One entry per universe ticker. The value is {:y annual-ds :q quarterly-ds},
  each the long dataset from `facts-ds/select-data` with every concept kept.
  The file is gzip around a Nippy payload."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.nippy :as nippy]
            [tech.v3.dataset.io.nippy]
            [edgar.api :as e]
            [edgar2.db.universe :as db-universe]
            [edgar2.download.facts :as dl]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.financials.facts-zip :as facts-zip]
            [edgar2.util.map-progress :refer [map-progress]])
  (:import [java.io DataInputStream DataOutputStream]
           [java.util.zip GZIPInputStream GZIPOutputStream]))

(def facts-path "data/facts.nippy.gz")

(defn load-facts
  "Thaw data/facts.nippy.gz. ticker → {:y annual-ds :q quarterly-ds}."
  []
  (with-open [in (-> (io/input-stream facts-path)
                     GZIPInputStream.
                     DataInputStream.)]
    (nippy/thaw-from-in! in)))

(defn- ticker-key
  [row]
  (let [t (some-> (:ticker row) str str/trim str/upper-case)]
    (when (and t (not (str/blank? t)))
      t)))

(defn- reports
  "Annual and quarterly facts for every concept on `dss`."
  [dss]
  {:y (facts-ds/select-data dss :y nil)
   :q (facts-ds/select-data dss :q nil)})

(defn convert-zip-to-dataset
  "Read data/companyfacts.zip and write data/facts.nippy.gz.

  Keys are universe tickers. Each value is {:y :q}, the `select-data`
  datasets for annual and quarterly periods (nil fields, so every concept).
  Tickers with no companyfacts entry are omitted. Returns the path.
  `clj -X` passes an option map; it is ignored."
  ([] (convert-zip-to-dataset {}))
  ([_]
  (e/init! dl/identity-header)
  (let [zip-path (dl/ensure-zip! {})
        rows (->> (db-universe/load-universe)
                  (keep (fn [row]
                          (when-let [t (ticker-key row)]
                            [t row])))
                  vec)
        total (count rows)
        t0 (System/nanoTime)
        _ (do (println (format "facts  %d tickers  ← %s" total zip-path))
              (flush))
        facts (with-open [zf (facts-zip/open-facts-zip)]
                (into {}
                      (keep (fn [{:keys [ticker built]}]
                              (when built
                                [ticker built])))
                      (map-progress
                       (fn [i total {:keys [ticker]}]
                         (format "  %d/%d  %s" i total ticker))
                       (fn [[ticker row]]
                         (let [dss (try (facts-zip/load-dataset zf (:cik row))
                                        (catch Exception ex
                                          (println (format "  skip %s (%s)" ticker (.getMessage ex)))
                                          nil))
                               built (when dss
                                       (try (reports dss)
                                            (catch Exception ex
                                              (println (format "  select %s (%s)" ticker (.getMessage ex)))
                                              nil)))]
                           {:ticker ticker :built built}))
                       rows)))
        file (io/file facts-path)
        tmp (io/file (str facts-path ".tmp"))
        mins (/ (double (- (System/nanoTime) t0)) 6.0e10)]
    (some-> (.getParentFile file) .mkdirs)
    (with-open [out (-> (io/output-stream tmp)
                        GZIPOutputStream.
                        DataOutputStream.)]
      (nippy/freeze-to-out! out facts))
    (.renameTo tmp file)
    (println (format "Wrote %s  (%d tickers, %.1fm)"
                     facts-path (count facts) mins))
    (flush)
    facts-path)))
