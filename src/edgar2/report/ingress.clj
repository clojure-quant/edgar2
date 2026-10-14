(ns edgar2.report.ingress
  "Refresh prices, listings, the two universes, and stats when they are stale."
  (:require [clojure.java.io :as io]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.facts-universe :as facts-universe]
            [edgar2.download.fsds-universe :as fsds]
            [edgar2.download.price :as price]
            [edgar2.download.ticker-exchange :as exchanges]
            [edgar2.report.stats :as stats])
  (:import [java.time LocalDate]
           [java.time.format DateTimeParseException]
           [java.time.temporal ChronoUnit]))

(def ^:dynamic *step*
  "When bound, called with {:event :start|:done :id :label} around each task.")

(def price-max-days 3)
(def listing-max-days 14)
(def fsds-max-days 14)
(def facts-max-days 3)

(defn- parse-date
  [s]
  (try
    (when s (LocalDate/parse s))
    (catch DateTimeParseException _ nil)))

(defn- age-days
  [date]
  (when-let [d (parse-date date)]
    (.between ChronoUnit/DAYS d (LocalDate/now))))

(defn- file-status
  "Due when the file or its :download-date is missing, or the date is older
  than `max-days`."
  [path max-days]
  (if-not (.exists (io/file path))
    {:due? true :date nil :age nil :quarter nil :summary "no file"}
    (let [header (dataset/peek-meta path)
          date (:download-date header)
          quarter (:download-quarter header)
          age (age-days date)
          base {:date date :age age :quarter quarter}]
      (cond
        (nil? date)
        (assoc base :due? true :summary "no date")
        (nil? age)
        (assoc base :due? true :summary (str "unreadable date " date))
        (> age max-days)
        (assoc base :due? true
               :summary (format "%s is %d %s old (refresh after %d)"
                                date age (if (= 1 age) "day" "days") max-days))
        :else
        (assoc base :due? false
               :summary (format "%s (%d %s old)"
                                date age (if (= 1 age) "day" "days")))))))

(defn plan
  "What ingress would do right now. Stats are due when any dataset is due,
  or when data/stats.edn is missing."
  []
  (let [prices (merge {:id :prices
                       :label "Prices"
                       :path price/prices-path
                       :max-days price-max-days
                       :as-of-label "session"
                       :run #(price/download-prices {})}
                      (file-status price/prices-path price-max-days))
        listings (merge {:id :exchanges
                         :label "Ticker exchange"
                         :path exchanges/exchanges-path
                         :max-days listing-max-days
                         :as-of-label "downloaded"
                         :run #(exchanges/download-exchanges {})}
                        (file-status exchanges/exchanges-path listing-max-days))
        fsds-item (merge {:id :fsds
                          :label "FSDS universe"
                          :path fsds/filer-info-path
                          :max-days fsds-max-days
                          :as-of-label "downloaded"
                          :run #(fsds/filer-info {})}
                         (file-status fsds/filer-info-path fsds-max-days))
        facts (merge {:id :facts
                      :label "Company facts"
                      :path facts-universe/universe-path
                      :max-days facts-max-days
                      :as-of-label "companyfacts published"
                      :run #(facts-universe/universe {:force true})}
                     (file-status facts-universe/universe-path facts-max-days))
        inputs [prices listings fsds-item facts]
        stats-file? (.exists (io/file stats/stats-path))
        stats-due? (or (some :due? inputs) (not stats-file?))]
    (conj (vec inputs)
          {:id :stats
           :label "Stats"
           :path stats/stats-path
           :due? stats-due?
           :date nil
           :summary (cond
                      (not stats-file?) "no file"
                      stats-due? "runs because a dataset will refresh"
                      :else "current")
           :run #(stats/stats {})})))

(defn run
  "Download each stale dataset, then rebuild stats if any dataset ran
  or stats are missing.

  No file and no :download-date both count as stale.
  Prices use the newest session (3 days). Ticker exchange and the FSDS
  universe use the day they were written (14 days). Company facts use the
  companyfacts.zip publication day (3 days) and are downloaded again before
  universe-facts is rebuilt.

  Usage: clj -X:ingress"
  ([] (run {}))
  ([_]
   (let [items (plan)]
     (doseq [item items]
       (println (format "%-16s %s  %s"
                        (:label item)
                        (if (:due? item) "DUE" "ok")
                        (:summary item))))
     (let [work (filterv :due? items)]
       (when (empty? work)
         (println "Nothing to do."))
       (doseq [item work]
         (when *step* (*step* {:event :start :id (:id item) :label (:label item)}))
         (println)
         (println (str "=== " (:label item) " ==="))
         (flush)
         ((:run item))
         (when *step* (*step* {:event :done :id (:id item)}))))
     items)))
