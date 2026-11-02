(ns edgar2.algo.industry
  "Ticker lists and Growth-style rows for one sector, industry, or
  subindustry selection."
  (:require [clojure.string :as str]
            [edgar2.algo.growth :as growth]
            [edgar2.report.screen :as report]))

(defn- normalize-filter
  "Trimmed, non-blank filter string, or nil."
  [value]
  (some-> value str str/trim not-empty))

(defn sector-tickers
  "Tickers of stats rows matching `filters`.

  `filters` may carry `:sector`, `:industry`, and `:subindustry`. A nil
  or blank value matches every row for that key. A set value must equal
  the row's value for that key. Tickers stay in stats-file order."
  ([filters]
   (sector-tickers (report/load-stats) filters))
  ([stats-rows filters]
   (let [wanted {:sector (normalize-filter (:sector filters))
                 :industry (normalize-filter (:industry filters))
                 :subindustry (normalize-filter (:subindustry filters))}]
     (->> stats-rows
          (filter (fn [row]
                    (every? (fn [[key value]]
                              (or (nil? value)
                                  (= value (get row key))))
                            wanted)))
          (keep :ticker)
          vec))))

(def columns
  "The Growth screen columns, in the same order."
  growth/columns)

(defn- future-yield
  [row]
  (let [y (:earnings-future-3-yield row)]
    (when (number? y) (double y))))

(defn rows
  "Growth-style table rows for `filters`.

  Full stats rows are looked up by ticker, sorted by
  `:earnings-future-3-yield` descending with missing values last, and
  converted with `edgar2.report.screen/table-row`."
  ([filters]
   (rows (report/load-stats) filters))
  ([stats-rows filters]
   (let [by-ticker (into {}
                         (keep (fn [row]
                                 (when-let [ticker (:ticker row)]
                                   [ticker row])))
                         stats-rows)
         hits (keep by-ticker (sector-tickers stats-rows filters))]
     (->> hits
          (sort-by #(or (future-yield %) Double/NEGATIVE_INFINITY) >)
          (mapv report/table-row)))))
