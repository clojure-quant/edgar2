(ns demo.13f
  "13F share counts for CUSIP 03748R747.

  Shares come from data/holdings.edn. Columns are the report periods
  where this CUSIP is held, oldest first, then a change column for each
  period after the first. Change is shares in that period minus shares
  in the prior period.

  A filer who filed a period and did not list the CUSIP holds 0.
  A filer with no filing for a period is blank for that period.

  A filer is kept on its own row only when every reported share count
  is at least 200,000 and every reported change has an absolute value
  of at least 200,000. Everyone else who reported a holding is summed
  into OTHER-INSTITUTIONS. Rows are sorted by shares in the latest
  report period, largest first.

  Usage:
    clj -M -e \"(require 'demo.13f) (demo.13f/run)\""
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.institutional :as institutional]))

(def cusip "03748R747")

(def ^:private threshold 200000)

(defn- load-filers
  []
  (with-open [r (io/reader institutional/holdings-path)]
    (:filers (edn/read (java.io.PushbackReader. r)))))

(defn- later-filing
  "The filing with the later :filing-date. Ties keep `b`."
  [a b]
  (if (pos? (compare (:filing-date a) (:filing-date b))) a b))

(defn- latest-filings
  "One filing per filer and report period: the latest :filing-date."
  [filers]
  (->> filers
       (filter :report-period)
       (group-by (juxt :filer :report-period))
       vals
       (map #(reduce later-filing %))))

(defn- qty
  "Shares of `cusips` in this filing. Zero when the filing lists none of them."
  [filing cusips]
  (reduce (fn [n h]
            (if (cusips (:cusip h))
              (+ n (or (:qty h) 0))
              n))
          0
          (:holdings filing)))

(defn- share-key [period] (keyword period))

(defn- change-key [period] (keyword (str period "C")))

(defn- columns
  [periods]
  (into [:filer-name]
        (concat (map share-key periods)
                (map change-key (rest periods)))))

(defn- shares-row
  [cusips periods by-period filer-name]
  (let [shares (into {:filer-name filer-name}
                     (map (fn [period]
                            (let [filing (get by-period period)]
                              [(share-key period)
                               (when filing (qty filing cusips))])))
                     periods)]
    (reduce (fn [row [prev cur]]
              (let [before (get row (share-key prev))
                    after (get row (share-key cur))]
                (assoc row (change-key cur)
                       (when (and before after) (- after before)))))
            shares
            (partition 2 1 periods))))

(defn- reported-holding?
  [row share-cols]
  (boolean (some #(and % (pos? %)) (map row share-cols))))

(defn- under-threshold?
  [n]
  (and (some? n) (< (abs n) threshold)))

(defn- aggregate?
  "Roll into OTHER-INSTITUTIONS when any reported share count is under
  200,000, or any period's share change is under 200,000 shares."
  [row share-cols change-cols]
  (boolean (or (some under-threshold? (map row share-cols))
               (some under-threshold? (map row change-cols)))))

(defn- sum-col
  [rows k]
  (let [xs (keep k rows)]
    (when (seq xs)
      (reduce + 0 xs))))

(defn- other-row
  [rows cols]
  (into {:filer-name "OTHER-INSTITUTIONS"}
        (map (fn [k] [k (sum-col rows k)]))
        cols))

(defn- by-latest
  [rows latest-key]
  (sort-by #(or (get % latest-key) Long/MIN_VALUE) #(compare %2 %1) rows))

(defn- cusips-in-holdings
  "CUSIPs from `cusips` that a latest filing actually lists. Order follows `cusips`."
  [filings cusips]
  (let [found (into #{}
                    (comp (mapcat :holdings) (map :cusip))
                    filings)]
    (filterv found cusips)))

(defn table
  "Filer rows for every CUSIP in `cusips` that holdings list.
  Returns {:cusips :periods :rows}."
  [filers cusips]
  (let [wanted (set cusips)
        latest (latest-filings filers)
        by-filer (group-by :filer latest)
        held (filter #(pos? (qty % wanted)) latest)
        periods (->> held (map :report-period) distinct sort vec)
        share-cols (mapv share-key periods)
        change-cols (mapv change-key (rest periods))
        value-cols (into share-cols change-cols)
        rows (->> (group-by :filer held)
                  (map (fn [[filer holding-filings]]
                         (let [by-period (into {} (map (juxt :report-period identity)
                                                      (get by-filer filer)))
                               name (:filer-name (reduce later-filing holding-filings))]
                           (shares-row wanted periods by-period name))))
                  (filter #(reported-holding? % share-cols)))
        {roll true keep false} (group-by #(aggregate? % share-cols change-cols) rows)
        shown (by-latest (concat keep
                                 (when (seq roll)
                                   [(other-row roll value-cols)]))
                         (last share-cols))]
    {:cusips (cusips-in-holdings held cusips)
     :periods periods
     :rows shown}))

(defn- cell
  [n]
  (if (nil? n) "" (format "%,d" (long n))))

(defn- format-row
  [row cols]
  (reduce (fn [row k] (update row k cell)) row cols))

(defn run
  "Load holdings, build the table for `cusip`, print it, and return the rows."
  ([] (run cusip))
  ([cusip]
   (let [{:keys [periods rows]} (table (load-filers) [cusip])
         cols (columns periods)]
     (println (format "cusip %s  %d report periods  %d rows"
                      cusip
                      (count periods)
                      (count rows)))
     (pprint/print-table cols (map #(format-row % (rest cols)) rows))
     (flush)
     rows)))

(defn -main
  [& _]
  (run))
