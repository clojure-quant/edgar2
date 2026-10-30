(ns edgar2.report.cef
  "Closed-end fund premium/discount from companyfacts CEF taxonomy."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar.api :as e]
            [edgar2.download.facts :as dl]
            [edgar2.db.universe :as db-universe]
            [edgar2.financials.facts-zip :as facts-zip]
            [edgar2.report.stats :as stats]
            [edgar2.util.map-progress :refer [map-progress]]))

(def cef-path "data/cef.edn")

(def cef-field-order
  [:ticker :name :exchange :date :d-low :d-high :d-last])

(def ^:private discount-concepts
  {:d-low  :LowestPriceOrBidPremiumDiscountToNavPercent
   :d-high :HighestPriceOrBidPremiumDiscountToNavPercent
   :d-last :LatestPremiumDiscountToNavPercent})

(defn- latest-obs
  "Most recent observation by :end from a CEF concept's :units map."
  [concept-map]
  (->> (:units concept-map)
       vals
       (mapcat identity)
       (sort-by :end)
       last))

(defn- cef-discounts
  "Premium/discount map from raw companyfacts `data`, or nil when no :cef taxonomy."
  [data]
  (when-let [cef (get-in data [:facts :cef])]
    (let [obs (into {}
                    (keep (fn [[out-k concept]]
                            (when-let [o (latest-obs (get cef concept))]
                              [out-k o]))
                          discount-concepts))
          date (->> (vals obs) (keep :end) sort last)]
      (cond-> {:date date}
        (contains? obs :d-low)  (assoc :d-low (:val (:d-low obs)))
        (contains? obs :d-high) (assoc :d-high (:val (:d-high obs)))
        (contains? obs :d-last) (assoc :d-last (:val (:d-last obs)))))))

(defn cef-one
  "One CEF row for universe `row` and companyfacts `data`, or nil if no CEF facts."
  [row data]
  (when-let [d (cef-discounts data)]
    (stats/select-fields
     (merge {:ticker (:ticker row)
             :name (:entityName row)
             :exchange (:exchange row)}
            d)
     cef-field-order)))

(defn save-cef!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str cef-path ".tmp")
        ordered (mapv #(stats/select-fields % cef-field-order) rows)]
    (spit tmp (with-out-str (pprint/pprint ordered)))
    (.renameTo (io/file tmp) (io/file cef-path))))

(defn cef
  "From data/universe-facts.edn + companyfacts.zip, write data/cef.edn for
  listed tickers that report the CEF taxonomy. Each row has :ticker, :name,
  :exchange, :date, and premium/discount to NAV:
    :d-low  LowestPriceOrBidPremiumDiscountToNavPercent
    :d-high HighestPriceOrBidPremiumDiscountToNavPercent
    :d-last LatestPremiumDiscountToNavPercent

  Usage: clj -X:cef
         clj -X:cef :limit 100"
  ([] (cef {}))
  ([{:keys [limit force]
     :or {force false}}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         all (db-universe/load-universe)
         filers (vec (cond->> all limit (take (long limit))))
         total (count filers)]
     (println (format "cef  scanning %d of %d universe  ← %s"
                      total (count all) zip-path))
     (flush)
     (with-open [zf (facts-zip/open-facts-zip)]
       (let [found (volatile! 0)
             hits (map-progress
                   (fn [i total {:keys [row]}]
                     (format "  %d/%d  found=%d  %s"
                             i total @found
                             (or (:ticker row) (:cik row))))
                   (fn [row]
                     (let [data (try (facts-zip/read-cik-json zf (:cik row))
                                    (catch Exception ex
                                      (println (format "  skip %s (%s)" (:cik row) (.getMessage ex)))
                                      nil))
                           st (when data (cef-one row data))]
                       (when st (vswap! found inc))
                       {:row row :st st}))
                   filers)
             out (->> hits
                      (keep :st)
                      (sort-by :ticker)
                      vec)]
         (save-cef! out)
         (println)
         (pprint/print-table
          cef-field-order
          (take 25 out))
         (println (format "Wrote %s  (%d cefs)" cef-path (count out)))
         out)))))
