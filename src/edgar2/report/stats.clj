(ns edgar2.report.stats
  "Universe fundamentals from companyfacts.zip: sales growth, margin, ROC."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.xbrl :as xbrl]
            [jsonista.core :as json]
            [edgar.api :as e]
            [edgar2.download.facts :as dl]
            [edgar2.download.price :as price]
            [edgar2.download.facts-universe :as universe]
            [edgar2.db.sic :as db-sic]
            [edgar2.db.universe :as db-universe]
            [edgar2.financials.calc.annual :as annual]
            [edgar2.financials.calc.valuation :as valuation])
  (:import [java.util.zip ZipFile]))

(def stats-path "data/stats.edn")

(def market-cap-adjusted
  "Delegates to `valuation/market-cap-adjusted` (web overview)."
  valuation/market-cap-adjusted)

(defn universe-data
  "Identity / reporting-currency fields from a universe-facts `row`."
  [row]
  (select-keys row [:ticker :exchange :cik :entityName :revenue-unit]))

(defn load-price-by-ticker
  "ticker → close from data/prices.edn."
  []
  (into {}
        (map (fn [{:keys [ticker close]}]
               [(str/upper-case (str ticker)) (double close)])
             (price/load-prices))))

(defn read-cik-json
  [zf cik]
  (when cik
    (let [entry (.getEntry zf (str "CIK" cik ".json"))]
      (when entry
        (with-open [in (.getInputStream zf entry)]
          (json/read-value in json/keyword-keys-object-mapper))))))

(def stats-field-order
  [:ticker :exchange :cik :entityName :sic :sector :industry :subindustry :price
   :shares :adr-ratio :marketcap :market-cap-adjusted :annual-count
   :revenue :revenue-unit
   :net-profit
   :eps
   :assets
   :return-on-assets
   :equity
   :return-on-capital
   :sales-growth-yoy
   :net-income-change-q-on-y
   :revenue-score
   :operating-income-score
   :price-earnings
   :dividends-paid
   :dividend-yield
   :dividend-coverage
   :stock-repurchase
   :buyback-yield
   :enterprise-value
   :ev-ebit
   :ebit
   :debt
   :cash])

(defn select-fields
  "Map with `ks` first (present keys only); remaining keys last, sorted."
  [m ks]
  (let [seen (set ks)
        front (mapcat (fn [k] (when (contains? m k) [k (get m k)])) ks)
        back (->> (keys m)
                  (remove seen)
                  sort
                  (mapcat (fn [k] [k (get m k)])))]
    (apply array-map (concat front back))))

(defn save-stats!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str stats-path ".tmp")
        ordered (mapv #(select-fields % stats-field-order) rows)]
    (spit tmp (with-out-str (pprint/pprint ordered)))
    (.renameTo (io/file tmp) (io/file stats-path))))

(defn fmt-growth
  [x]
  (when x (format "%.1f" (double x))))

(defn round1
  [x]
  (when x (/ (Math/round (* (double x) 10.0)) 10.0)))

(defn round4
  [x]
  (when x (/ (Math/round (* (double x) 10000.0)) 10000.0)))

(defn stats-one
  "One stats row for `cik` from companyfacts dataset `dss` (nil when missing).

  Runs `annual/calculate-annual` (when `dss` is present), market valuation
  (including ADR ratio), and SIC enrichment. Looks up the universe row by
  `cik` unless `:row` is passed. Optional opts for the batch path: `:row`,
  `:prices`, `:sic-db`, `:shares`, `:data` (raw companyfacts map for
  shares-outstanding)."
  ([dss cik]
   (stats-one dss cik {}))
  ([dss cik {:keys [row prices sic-db shares data]}]
   (let [row (or row (db-universe/universe-row-by-cik cik) {:cik (str cik)})
         prices (or prices (load-price-by-ticker))
         sic-db (or sic-db (db-sic/load-sic-index))
         shares (or shares
                    (when data (universe/shares-outstanding (:facts data)))
                    (:shares-outstanding row))]
     (try
       (let [annual-map (if dss (annual/calculate-annual dss) {})
             mc-map (valuation/calculate-marketcap prices shares row)
             valuation-map (valuation/calculate-valuation annual-map (:vmcap mc-map))]
         (merge annual-map
                (universe-data row)
                (dissoc mc-map :vmcap)
                valuation-map
                (db-sic/sic-data cik sic-db)))
       (catch Exception ex
         (println (format "  stats-fail %s (%s)"
                          (or (:ticker row) cik) (.getMessage ex)))
         (merge (universe-data row) (db-sic/sic-data cik sic-db)))))))

(defn stats
  "From data/universe-facts.edn + companyfacts.zip + prices.edn, write data/stats.edn
  with 7-year mean YoY sales growth (%),
  :net-income-change-q-on-y (latest quarter NI vs NI 4 quarters earlier, %,
  |prior| denominator), profit margin, 10-year mean
  profit margin (Net Income / revenue), return on assets (Net Income / assets),
  return on capital (Net Income / total equity), plus :shares (most recent 10-Q
  when that quarter is at least as recent as the annual report; otherwise
  the annual report), :annual-count (distinct annual periods in companyfacts),
  :price, :marketcap, :price-sales,
  :price-earnings (USD market cap / USD Net Income, else USD price / USD EPS),
  :dividend-yield (USD dividends paid / USD market cap, else USD dividend per share / price),
  :dividend-coverage (net income / dividends paid; omitted when none was paid),
  :buyback-yield (USD stock repurchase / USD market cap),
  :enterprise-value (USD market cap + debt + preferred + NCI − cash),
  :ev-ebit, :revenue-score, and :operating-income-score
  (0–100 share of annual years that did not fall versus the prior year).
  When operating income is not tagged, a year whose operating expenses were
  derived uses gross profit minus those expenses.
  :adr-ratio (ordinary shares per ADR from data/adr-list.edn) when known.
  :market-cap-adjusted (reporting-currency market cap after ADR) when FX allows.
  Non-USD or non-1 ADR names use that cap for price-sales, PE, yields, and EV.
  :sic, :sector, :industry, and :subindustry come from data/universe-fsds.edn.

  Usage: clj -X:stats
         clj -X:stats :limit 25"
  ([] (stats {}))
  ([{:keys [limit force]
     :or {force false}}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         all (db-universe/load-universe)
         prices (load-price-by-ticker)
         sic-db (db-sic/load-sic-index)
         filers (vec (cond->> all limit (take (long limit))))
         total (count filers)]
     (println (format "stats  %d of %d universe  ← %s"
                      total (count all) zip-path))
     (flush)
     (with-open [zf (ZipFile. (str zip-path))]
       (loop [i 0
              acc (transient [])
              rows filers]
         (if-let [row (first rows)]
           (let [data (try (read-cik-json zf (:cik row))
                           (catch Exception ex
                             (println (format "  skip %s (%s)" (:cik row) (.getMessage ex)))
                             nil))
                 dss (when data
                       (try (xbrl/facts->dataset data)
                            (catch Exception ex
                              (println (format "  facts-ds %s (%s)" (:cik row) (.getMessage ex)))
                              nil)))
                 shares (or (when data
                              (universe/shares-outstanding (:facts data)))
                            (:shares-outstanding row))
                 st (stats-one dss (:cik row)
                               {:row row
                                :prices prices
                                :sic-db sic-db
                                :shares shares
                                :data data})
                 i' (inc i)]
             (when (or (zero? (mod i' 250)) (= i' total))
               (println (format "  %d/%d  %s  growth=%s  margin=%s  roa=%s  roc=%s"
                                i' total
                                (or (:ticker st) (:cik st))
                                (fmt-growth (:sales-growth-yoy st))
                                (some-> (:profit-margin st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-assets st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-capital st) (* 100) (#(format "%.1f" %)))))
               (flush))
             (recur i' (conj! acc st) (next rows)))
           (let [out (vec (persistent! acc))]
             (save-stats! out)
             (println)
             (pprint/print-table
              [:ticker :price :marketcap :price-sales :price-earnings
               :sales-growth-yoy :profit-margin :return-on-assets :return-on-capital]
              (take 25
                    (map (fn [r]
                           (-> r
                               (update :sales-growth-yoy round1)
                               (update :profit-margin round4)
                               (update :return-on-assets round4)
                               (update :return-on-capital round4)
                               (update :price-sales round4)
                               (update :price-earnings round4)))
                         out)))
             (println (format "Wrote %s  (%d companies; growth=%d  margin=%d  avg-margin=%d  roa=%d  roc=%d  price=%d  mcap=%d  adr=%d  pe=%d  ev=%d  ev-ebit=%d  div-coverage=%d  buyback=%d  rev-score=%d  oi-score=%d  sic=%d)"
                              stats-path
                              (count out)
                              (count (filter :sales-growth-yoy out))
                              (count (filter :profit-margin out))
                              (count (filter :avg-profit-margin out))
                              (count (filter :return-on-assets out))
                              (count (filter :return-on-capital out))
                              (count (filter :price out))
                              (count (filter :marketcap out))
                              (count (filter :adr-ratio out))
                              (count (filter :price-earnings out))
                              (count (filter :enterprise-value out))
                              (count (filter :ev-ebit out))
                              (count (filter :dividend-coverage out))
                              (count (filter :buyback-yield out))
                              (count (filter :revenue-score out))
                              (count (filter :operating-income-score out))
                              (count (filter :sic out))))
             out)))))))
