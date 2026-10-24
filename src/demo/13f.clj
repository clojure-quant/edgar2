(ns demo.13f
  "13F share counts for CUSIP 03748R747.

  Shares come from data/holdings.nippy. Columns are the report periods
  where this CUSIP is held, from 2025-12-31 onward, oldest first, then a
  change column for each period after the first. Change is shares in
  that period minus shares in the prior period.

  A filer who filed a period and did not list the CUSIP holds 0.
  A filer with no filing for a period is blank for that period.
  Change is calculated only when both periods have a number. If either
  is blank, the change is blank. Blank cells print as an empty string.

  A filer is kept on its own row when the largest reported position is
  at least 500,000 shares, or any period-to-period change has an
  absolute value of at least 200,000. Everyone else who reported a
  holding is summed into OTHER-INSTITUTIONS. Rows are sorted by shares
  in the latest report period, largest first.

  Usage:
    clj -M:13F"
  (:require [clojure.pprint :as pprint]
            [edgar2.db.holdings :as holdings]))

(def cusip "03748R747")

(def ^:private first-period "2025-12-31")

(def ^:private min-position 500000)

(def ^:private min-move 200000)

(defn- kept-period?
  "True for 2025-12-31 and every later report period."
  [period]
  (not (neg? (compare period first-period))))

(defn- load-filers
  []
  (holdings/load))

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
                       (when (and (some? before) (some? after))
                         (- after before)))))
            shares
            (partition 2 1 periods))))

(defn- reported-holding?
  [row share-cols]
  (boolean (some #(and % (pos? %)) (map row share-cols))))

(defn- own-row?
  "Keep a filer on its own row when its largest reported position is at
  least 500,000 shares, or any period-to-period change is at least
  200,000 shares."
  [row share-cols change-cols]
  (let [positions (keep row share-cols)]
    (boolean (or (when (seq positions)
                   (>= (apply max positions) min-position))
                 (some (fn [n]
                         (and (some? n) (>= (abs n) min-move)))
                       (map row change-cols))))))

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
        held (->> latest
                  (filter #(and (kept-period? (:report-period %))
                                (pos? (qty % wanted)))))
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
        {keep true roll false} (group-by #(own-row? % share-cols change-cols) rows)
        shown (by-latest (concat keep
                                 (when (seq roll)
                                   [(other-row roll value-cols)]))
                         (last share-cols))]
    {:cusips (cusips-in-holdings held cusips)
     :periods periods
     :rows shown}))

(defn- cell
  "Blank for nil. Zero and every other number print with thousands separators."
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
