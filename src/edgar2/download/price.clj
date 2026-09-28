(ns edgar2.download.price
  "US stock session closes from MarketParquet daily bars → data/prices.edn.

  Without MARKETPARQUET_API_KEY this uses the public latest stock_daily file
  (one close per symbol for the newest session). A key unlocks
  GET /api/v1/download/stock_daily/{date}."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [jsonista.core :as json])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers]
           [java.sql DriverManager]))

(def prices-path "data/prices.edn")

(def api-base "https://marketparquet.com/api/v1")

(def public-latest-url
  "https://marketparquet.com/api/data/download/stock_daily/latest.parquet")

(defn api-key
  []
  (let [k (System/getenv "MARKETPARQUET_API_KEY")]
    (when-not (str/blank? k) k)))

(defn sql-quote
  [s]
  (str "'" (str/replace (str s) "'" "''") "'"))

(defn http-client
  []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NEVER)
      (.build)))

(defn header
  [resp name]
  (-> resp .headers (.firstValue name) (.orElse "")))

(defn resolve-redirect
  [base loc]
  (str (.resolve (URI/create base) loc)))

(defn build-request
  [url headers]
  (let [b (-> (HttpRequest/newBuilder)
              (.uri (URI/create url))
              (.GET))]
    (.build (reduce (fn [b [k v]] (.header b (str k) (str v))) b headers))))

(defn auth-headers
  []
  (if-let [k (api-key)]
    {"Authorization" (str "Bearer " k)
     "Accept" "application/json"}
    {"Accept" "application/json"}))

(defn fetch-json
  [url headers]
  (let [resp (.send (http-client)
                    (build-request url headers)
                    (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (when-not (= 200 status)
      (throw (ex-info (str "MarketParquet API failed: HTTP " status)
                      {:url url :status status :body (.body resp)})))
    (json/read-value (.body resp) json/keyword-keys-object-mapper)))

(defn response-step
  "[:redirect url] or [:file resp]. Caller owns a :file body."
  [resp url]
  (let [status (.statusCode resp)
        ctype (str/lower-case (header resp "content-type"))]
    (cond
      (#{301 302 303 307 308} status)
      (let [loc (header resp "location")]
        (.close (.body resp))
        (when (str/blank? loc)
          (throw (ex-info "Redirect without Location" {:url url :status status})))
        [:redirect (resolve-redirect url loc)])

      (and (= 200 status) (str/includes? ctype "json"))
      (with-open [in (.body resp)]
        (let [m (json/read-value (slurp in) json/keyword-keys-object-mapper)
              next-url (:download_url m)]
          (when-not next-url
            (throw (ex-info "MarketParquet response had no download_url"
                            {:url url :body m})))
          [:redirect next-url]))

      (= 200 status)
      [:file resp]

      :else
      (with-open [in (.body resp)]
        (let [body (slurp in)]
          (throw (ex-info (str "MarketParquet download failed: HTTP " status)
                          {:url url
                           :status status
                           :body (subs body 0 (min 500 (count body)))})))))))

(defn download-file!
  "GET `url` to `dest`. Follows redirects and a JSON `download_url`, and
  drops request headers on the way so a Bearer token is not sent to storage."
  [url dest headers]
  (.mkdirs (.getParentFile (io/file dest)))
  (let [client (http-client)]
    (loop [url url headers headers hops 0]
      (when (> hops 5)
        (throw (ex-info "Too many redirects" {:url url})))
      (let [resp (.send client
                        (build-request url headers)
                        (HttpResponse$BodyHandlers/ofInputStream))
            [step payload] (response-step resp url)]
        (case step
          :redirect (recur payload {} (inc hops))
          :file (with-open [in (.body payload)
                            out (java.io.FileOutputStream. dest)]
                  (io/copy in out)
                  dest))))))

(defn latest-session-date
  []
  (let [payload (fetch-json (str api-base "/dates/stock_daily?page=1")
                            (auth-headers))
        date (:date (first (:dates payload)))]
    (when (str/blank? (str date))
      (throw (ex-info "MarketParquet returned no stock_daily dates" {:body payload})))
    date))

(defn parquet-url
  "stock_daily download URL. Without an API key, only the public latest file."
  [date]
  (if (api-key)
    (str api-base "/download/stock_daily/" (or date (latest-session-date)))
    (do
      (when date
        (throw (ex-info "A :date needs MARKETPARQUET_API_KEY. Without a key only the latest US session is public."
                        {:date date})))
      public-latest-url)))

(defn ensure-duckdb!
  []
  (try
    (Class/forName "org.duckdb.DuckDBDriver")
    (catch ClassNotFoundException _
      (throw (ex-info "Reading MarketParquet needs DuckDB JDBC. Run: clj -X:download-prices"
                      {})))))

(defn duck-query
  [sql]
  (ensure-duckdb!)
  (with-open [conn (DriverManager/getConnection "jdbc:duckdb:")
              stmt (.createStatement conn)
              rs (.executeQuery stmt sql)]
    (let [md (.getMetaData rs)
          n (.getColumnCount md)
          names (mapv #(keyword (.getColumnLabel md (inc %))) (range n))]
      (loop [rows []]
        (if (.next rs)
          (recur (conj rows
                       (into {}
                             (map (fn [i col]
                                    [col (.getObject rs (inc i))])
                                  (range n)
                                  names))))
          rows)))))

(defn date-expr
  [columns]
  (cond
    (columns "date") "CAST(date AS VARCHAR)"
    (columns "timestamp") "CAST(CAST(\"timestamp\" AS DATE) AS VARCHAR)"
    :else (throw (ex-info "MarketParquet file has no date or timestamp column"
                          {:columns columns}))))

(defn close-rows
  "One {:ticker :date :close} per symbol from a stock_daily parquet file."
  [parquet-path]
  (let [qpath (sql-quote parquet-path)
        columns (->> (duck-query (str "DESCRIBE SELECT * FROM read_parquet(" qpath ")"))
                     (map #(str (:column_name %)))
                     set)
        sql (str "SELECT upper(symbol) AS ticker, "
                 (date-expr columns) " AS date, close "
                 "FROM read_parquet(" qpath ") "
                 "WHERE symbol IS NOT NULL AND close IS NOT NULL AND isfinite(close) "
                 "ORDER BY 1")]
    (mapv (fn [{:keys [ticker date close]}]
            {:ticker (str ticker)
             :date (str date)
             :close (double close)})
          (duck-query sql))))

(defn save-prices!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str prices-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file prices-path))))

(defn load-prices
  "Rows from data/prices.edn."
  []
  (if (.exists (io/file prices-path))
    (edn/read-string (slurp prices-path))
    (throw (ex-info "Missing prices file; run clj -X:download-prices first"
                    {:path prices-path}))))

(defn normalize-date
  [date]
  (when date
    (let [s (str date)]
      (when-not (re-matches #"\d{4}-\d{2}-\d{2}" s)
        (throw (ex-info "Price :date must be YYYY-MM-DD" {:date date})))
      s)))

(defn download-prices
  "Download US stock closes from MarketParquet and write data/prices.edn.

  Usage: clj -X:download-prices
         clj -X:download-prices :date '\"2026-09-25\"'"
  ([] (download-prices {}))
  ([{:keys [date]}]
   (let [date (normalize-date date)
         url (parquet-url date)
         tmp (.getAbsolutePath (io/file "data" "stock_daily.parquet.part"))]
     (println (str "MarketParquet stock_daily"
                   (when date (str "  " date))
                   (when-not (api-key) "  (public latest)")))
     (println (str "  → " url))
     (flush)
     (try
       (download-file! url tmp (if (str/starts-with? url api-base)
                                 (auth-headers)
                                 {}))
       (let [rows (close-rows tmp)
             session (some :date rows)]
         (save-prices! rows)
         (println (format "US closes: %d symbols  session %s" (count rows) session))
         (println (format "Wrote %s" prices-path))
         (println)
         (pprint/print-table [:ticker :date :close] (take 25 rows))
         (println (format "... %d more (see %s)"
                          (max 0 (- (count rows) 25))
                          prices-path))
         rows)
       (finally
         (.delete (io/file tmp)))))))
