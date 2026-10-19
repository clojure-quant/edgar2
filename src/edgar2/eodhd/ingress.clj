(ns edgar2.eodhd.ingress
  "US equities from EODHD, then 50 years of daily bars and splits.

  The token is :eodhd in $QUANTASTORE/edgar2.edn. Equities are common and
  preferred stock (quanta.eodhd.ds/convert-asset category :equity). The
  due download keeps names with close × volume of at least 20 million;
  run-all / Download all keeps every equity.

  Bars and splits are written as nippy datasets under data/eodhd/bardb,
  one series per symbol, the same file layout as quanta.bar.db.nippy:
  AAPL-us-d.nippy.gz for bars and _split_AAPL-us-d.nippy.gz for splits.
  Split files use the columns written by quanta.bar.split.service
  (open/high/low/close all equal the split factor). Downloads go through
  quanta.eodhd and the same rate limit as the library's list import.

  Usage: clj -X:ingress-eodhd
         clj -X:ingress-all"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [missionary.core :as m]
            [quanta.bar.db.nippy :as nippy]
            [quanta.bar.protocol :as b]
            [quanta.bar.split.service :as split]
            [quanta.eodhd.ds :as ds]
            [quanta.eodhd.eod-close-time :refer [adjust-time-to-exchange-close]]
            [quanta.eodhd.raw :as raw]
            [quanta.eodhd.util.date :as eod-date]
            [quanta.missionary :refer [rest-import]]
            [tablecloth.api :as tc]
            [taoensso.timbre :as timbre]
            [tech.v3.dataset :as tds]
            [tech.v3.dataset.io.nippy]
            [tick.core :as t])
  (:import [java.io FileNotFoundException]
           [java.time LocalDate]
           [java.time.format DateTimeParseException]
           [java.time.temporal ChronoUnit]))

;; tech.v3.dataset.io.nippy registers freeze and thaw for datasets.
;; quanta.bar.db.nippy writes those datasets, and the screen reads them back.

(def ^:dynamic *step*
  "When bound, called with {:event :start|:done :id :label} around each task."
  nil)

(def turnover-min
  "Minimum close × volume, in dollars, on the latest bulk session."
  20000000.0)

(def history-years 50)
(def max-days 3)
(def calendar [:us :d])

(def equities-path "data/eodhd/equities.edn")
(def bars-meta-path "data/eodhd/bars.edn")
(def splits-meta-path "data/eodhd/splits.edn")
(def bardb-path "data/eodhd/bardb/")

(defonce ^:private bardb*
  (delay (nippy/start-bardb-nippy bardb-path)))

(defn- bardb [] @bardb*)

(defonce ^:private split-service*
  (delay (split/start-split-service {:bardb (bardb)})))

(defn- split-service [] @split-service*)

(defn bar-file
  "Nippy path for one symbol. quanta.bar.db.nippy names files the same way."
  [asset]
  (str bardb-path (str/replace asset #"/" "_") "-us-d.nippy.gz"))

(defn- split-missing?
  [ex]
  (loop [e ex]
    (cond
      (nil? e) false
      (and (instance? FileNotFoundException e)
           (str/includes? (str e) "_split_")) true
      :else (recur (ex-cause e)))))

(defn adjusted-bars
  "Split-adjusted daily bars for `asset`.

  Reads through quanta.bar.split.service. A symbol with no split file is
  already unadjusted, which is the same as a factor of 1."
  [asset]
  (let [path (bar-file asset)]
    (when (.exists (io/file path))
      (try
        (m/? (b/get-bars (:barsource (split-service))
                         {:asset asset :calendar calendar}
                         {}))
        (catch Exception ex
          (if (split-missing? ex)
            (nippy/load-ds path)
            (throw ex)))))))

(defn- quiet-logs!
  "The EODHD client logs the token at info. Keep it out of the ingress log."
  []
  (timbre/set-min-level! :warn))

(defn- token []
  (let [root (System/getenv "QUANTASTORE")
        path (io/file root "edgar2.edn")]
    (when (or (str/blank? root) (not (.exists path)))
      (throw (ex-info "Missing $QUANTASTORE/edgar2.edn (:eodhd token)"
                      {:path (str path)})))
    (let [value (:eodhd (edn/read-string (slurp path)))]
      (when (str/blank? value)
        (throw (ex-info "No :eodhd token in edgar2.edn" {:path (str path)})))
      value)))

(defn- today []
  (str (LocalDate/now)))

(defn- parse-date [s]
  (try
    (when s (LocalDate/parse s))
    (catch DateTimeParseException _ nil)))

(defn- age-days [date]
  (when-let [d (parse-date date)]
    (.between ChronoUnit/DAYS d (LocalDate/now))))

(defn- day-word [n]
  (if (= 1 n) "day" "days"))

(defn- session-window []
  (let [end (LocalDate/now)
        start (.minusYears end history-years)]
    {:start (t/zoned-date-time (str start "T00:00:00Z"))
     :end (t/zoned-date-time (str end "T00:00:00Z"))}))

(defn- order-dates [ds]
  (if (pos? (tc/row-count ds))
    (tc/order-by ds [:date])
    ds))

(defn- equity? [asset]
  (= :equity (:asset/category (ds/convert-asset asset))))

(defn- reject-bad-token [ex]
  (let [msg (or (ex-message ex) "")]
    (if (str/includes? (str/lower-case msg) "unauthorized")
      (ex-info "EODHD rejected the token in $QUANTASTORE/edgar2.edn" {})
      ex)))

(defn import-equities
  "Write data/eodhd/equities.edn. Returns the rows, highest turnover first.

  With no args (or :min-turnover matching turnover-min), keeps equities whose
  latest bulk session has close × volume of at least that floor. Pass
  :min-turnover 0 to keep every US common and preferred stock."
  ([] (import-equities {}))
  ([{:keys [min-turnover] :or {min-turnover turnover-min}}]
   (quiet-logs!)
   (let [min-turnover (double (or min-turnover 0.0))
         all? (not (pos? min-turnover))]
     (println (if all?
                "Importing all US common and preferred stocks"
                (format "Importing US equities with turnover >= %.0f million"
                        (/ min-turnover 1.0e6))))
     (flush)
     (let [api-token (token)
           assets (try
                    (m/? (raw/get-exchange-assets api-token "US"))
                    (catch Exception ex
                      (throw (reject-bad-token ex))))
           equities (filterv equity? assets)
           bulk (m/? (raw/get-day-bulk api-token {:exchange "US"}))
           bulk-by-code (into {}
                              (keep (fn [row]
                                      (when (string? (:code row))
                                        [(:code row) row])))
                              bulk)
           rows (->> equities
                     (keep (fn [asset]
                             (let [code (:Code asset)
                                   row (get bulk-by-code code)
                                   close (:close row)
                                   volume (:volume row)
                                   turnover (if (and (number? close) (number? volume))
                                              (* (double close) (double volume))
                                              0.0)]
                               (when (or all? (>= turnover min-turnover))
                                 {:code code
                                  :name (or (:Name asset) "")
                                  :exchange (:Exchange asset)
                                  :turnover (long (Math/round turnover))}))))
                     (sort-by :turnover >)
                     vec)]
       (when (empty? rows)
         (throw (ex-info (if all?
                           "No US equities from EODHD"
                           (format "No US equities with turnover of at least %.0f"
                                   min-turnover))
                         {})))
       (dataset/write-dataset! equities-path {:download-date (today)} rows)
       (println (format "%d equities" (count rows)))
       (flush)
       rows))))

(defn- bars-dataset
  "Same shaping as quanta.eodhd.ds/get-bars-eodhd, with the rows already in hand.
  The library call uses a 5 second socket timeout, which is too short for
  50 years of daily bars."
  [calendar rows]
  (let [ds (tds/->dataset rows)]
    (-> ds
        (tc/add-column :date (map eod-date/parse-date-only (:date ds)))
        (adjust-time-to-exchange-close (first calendar))
        order-dates)))

(defn- download-bars [ctx opts]
  (m/sp
   (let [rows (m/? (raw/eodhd-http-get
                    (:eodhd-token ctx)
                    (str "eod/" (:asset opts))
                    {:order "a"
                     :period "d"
                     :from (ds/fmt-yyyymmdd (:start opts))
                     :to (ds/fmt-yyyymmdd (:end opts))
                     :socket-timeout 120000
                     :connection-timeout 30000}))
         _ (when-not (sequential? rows)
             (throw (ex-info (str "bars failed for " (:asset opts))
                             {:asset (:asset opts)})))
         _ (when-let [w (and (seq rows) (:warning (last rows)))]
             (throw (ex-info (str w) {:asset (:asset opts)})))
         bars (if (seq rows)
                (bars-dataset (:calendar opts) rows)
                (tc/dataset))]
     bars)))

(defn- store-bars [_ctx opts data]
  (m/sp
   (when (and data (pos? (tc/row-count data)))
     (let [opts (select-keys opts [:asset :calendar])]
       (m/? (b/delete-bars (bardb) opts))
       (m/? (b/append-bars (bardb) opts data))))))

(defn- download-splits [ctx opts]
  (m/sp
   (let [splits (order-dates
                 (m/? (ds/get-splits (:eodhd-token ctx)
                                     (select-keys opts [:asset :calendar])
                                     (select-keys opts [:start :end]))))]
     splits)))

(defn- store-splits [_ctx opts data]
  (m/sp
   (when (and data (pos? (tc/row-count data)))
     ;; save-splits omits :asset, so nippy would write every symbol into one
     ;; file. The columns match save-splits; the filename is what get-splits
     ;; reads back (_split_SYMBOL-us-d.nippy.gz).
     (let [asset (:asset opts)
           prefixed (str "_split_" asset)
           factors (:factor data)
           stored (-> data
                      (tc/add-column :asset prefixed)
                      (tc/rename-columns {:factor :close})
                      (tc/add-columns {:open factors
                                       :high factors
                                       :low factors
                                       :volume 0}))]
       (m/? (split/delete-splits (split-service) asset))
       (m/? (b/append-bars (bardb) {:asset prefixed :calendar calendar} stored))))))

(def ^:private import-limits
  {;; Slower than the library's list import. A burst of 600 drew HTTP 429s.
   :parallel 8
   :cost 1
   :capacity 20
   :rate 5})

(defn- with-progress
  "Wrap a download-fn so every 100 finished series print `n/total downloaded`."
  [download-fn total]
  (let [done (atom 0)
        total (long total)]
    (fn [ctx opts]
      (m/sp
       (try
         (m/? (download-fn ctx opts))
         (finally
           (let [n (swap! done inc)]
             (when (or (zero? (mod n 100)) (= n total))
               (println (format "%d/%d downloaded" n total))
               (flush)))))))))

(defn- download-all [equities download-fn store-fn]
  (quiet-logs!)
  (let [api-token (token)
        window (session-window)
        ctx {:eodhd-token api-token}
        tasks (mapv (fn [{:keys [code]}]
                      (merge {:asset code :calendar calendar} window))
                    equities)
        n (count tasks)]
    (println (format "0/%d downloaded" n))
    (flush)
    (m/? (rest-import ctx
                      (merge import-limits
                             {:tasks-opts tasks
                              :download-fn (with-progress download-fn n)
                              :store-fn store-fn})))))

(defn- public-error [s]
  (str/replace (str s) #"api_token=[^&\s\"]+" "api_token=***"))

(defn- report! [label result]
  (println (format "%s: %d ok, %d errors" label (:success result) (:error result)))
  (doseq [detail (take 30 (:error-details result))]
    (println "ERROR" (:asset detail) (public-error (:error detail))))
  (flush)
  result)

(defn- write-manifest! [path result n]
  (dataset/write-dataset! path
                          {:download-date (today)}
                          [{:assets n
                            :success (or (:success result) 0)
                            :error (or (:error result) 0)
                            :history-years history-years}]))

(defn- finish! [label path result n]
  (report! label result)
  (when (and (pos? n)
             (zero? (long (or (:success result) 0)))
             (pos? (long (or (:error result) 0))))
    (let [sample (:error (first (:error-details result)))]
      (throw (ex-info (str "All " n " requests failed"
                           (when sample (str ". " (public-error sample))))
                      {}))))
  (write-manifest! path result n)
  result)

(defn bar-download
  ([] (bar-download {}))
  ([opts]
   (let [equities (import-equities opts)]
     (finish! "Bar download" bars-meta-path
              (download-all equities download-bars store-bars)
              (count equities)))))

(defn split-download
  ([] (split-download {}))
  ([opts]
   (let [equities (import-equities opts)]
     (finish! "Split download" splits-meta-path
              (download-all equities download-splits store-splits)
              (count equities)))))

(defn- manifest-status
  [path]
  (if-not (.exists (io/file path))
    {:due? true :date nil :summary "no file"}
    (let [date (:download-date (dataset/peek-meta path))
          age (age-days date)
          stats (first (dataset/read-rows path))
          counts (when stats
                   (format "%d equities, %d ok, %d errors"
                           (or (:assets stats) 0)
                           (or (:success stats) 0)
                           (or (:error stats) 0)))
          history-ok? (= history-years (:history-years stats))
          with-counts (fn [text]
                        (if counts (str text "; " counts) text))]
      (cond
        (nil? date)
        {:due? true :date nil :summary "no date"}

        (nil? age)
        {:due? true :date date :summary (str "unreadable date " date)}

        (not history-ok?)
        {:due? true
         :date date
         :summary (with-counts (str history-years " years not downloaded"))}

        (> age max-days)
        {:due? true
         :date date
         :summary (with-counts
                    (format "%s is %d %s old (refresh after %d)"
                            date age (day-word age) max-days))}

        :else
        {:due? false
         :date date
         :summary (with-counts
                    (format "%s (%d %s old)"
                            date age (day-word age)))}))))

(defn plan
  []
  [(merge {:id :bar-download
           :label "Bar download"
           :path bars-meta-path
           :max-days max-days
           :run bar-download}
          (manifest-status bars-meta-path))
   (merge {:id :split-download
           :label "Split download"
           :path splits-meta-path
           :max-days max-days
           :run split-download}
          (manifest-status splits-meta-path))])

(defn- run-items
  [items work]
  (doseq [item items]
    (println (format "%-16s %s  %s"
                     (:label item)
                     (if (:due? item) "DUE" "ok")
                     (:summary item))))
  (when (empty? work)
    (println "Nothing to do."))
  (doseq [item work]
    (when *step* (*step* {:event :start :id (:id item) :label (:label item)}))
    (println)
    (println (str "=== " (:label item) " ==="))
    (flush)
    ((:run item))
    (when *step* (*step* {:event :done :id (:id item)})))
  items)

(defn run
  "Import the equity list, then download whichever of bars and splits is stale.

  Usage: clj -X:ingress-eodhd"
  ([] (run {}))
  ([_]
   (quiet-logs!)
   (let [items (plan)]
     (run-items items (filterv :due? items)))))

(defn run-all
  "Import every US equity, then download bars and splits for all of them.

  Unlike `run`, this ignores the $20M turnover floor (~2k names) and pulls
  the full common/preferred list (~18k). Also re-downloads even when fresh.

  Usage: clj -X:ingress-all"
  ([] (run-all {}))
  ([_]
   (quiet-logs!)
   (let [opts {:min-turnover 0}
         items (plan)
         work [(assoc (first items) :run #(bar-download opts))
               (assoc (second items) :run #(split-download opts))]]
     (run-items items work))))
