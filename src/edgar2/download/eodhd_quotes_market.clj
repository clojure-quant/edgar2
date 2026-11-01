(ns edgar2.download.eodhd-quotes-market
  "US entire-market closes from one EODHD bulk last-day request → data/prices-eodhd.edn.

  One request: GET /api/eod-bulk-last-day/US. Rows have the same shape as
  data/prices.edn: {:ticker :date :close}.

  The token is :eodhd in $QUANTASTORE/edgar2.edn.

  Usage: clj -X:download-eodhd-quotes-market"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [jsonista.core :as json])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]))

(def prices-path "data/prices-eodhd.edn")

(def exchange "US")

(def endpoint "https://eodhd.com/api/eod-bulk-last-day/")

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

(defn- http-client []
  (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofSeconds 30))
      (.build)))

(defn- bulk-url
  [api-token]
  (str endpoint exchange
       "?api_token=" (URLEncoder/encode api-token StandardCharsets/UTF_8)
       "&fmt=json"))

(defn- public-error
  [s]
  (str/replace (str s) #"api_token=[^&\s\"]+" "api_token=***"))

(defn- fetch-last-day
  "Every US symbol EODHD published for the most recent session."
  [api-token]
  (let [resp (.send (http-client)
                    (-> (HttpRequest/newBuilder)
                        (.uri (URI/create (bulk-url api-token)))
                        (.timeout (Duration/ofSeconds 180))
                        (.header "Accept" "application/json")
                        (.GET)
                        .build)
                    (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)
        body (.body resp)]
    (cond
      (= 401 status)
      (throw (ex-info "EODHD rejected the token in $QUANTASTORE/edgar2.edn" {}))

      (not= 200 status)
      (throw (ex-info (str "EODHD bulk last-day HTTP " status)
                      {:status status
                       :body (public-error (subs body 0 (min 500 (count body))))}))

      :else
      (let [parsed (json/read-value body json/keyword-keys-object-mapper)]
        (when-not (sequential? parsed)
          (throw (ex-info "EODHD bulk last-day was not a list"
                          {:body (public-error (subs body 0 (min 500 (count body))))})))
        (vec parsed)))))

(defn- close-row
  "One {:ticker :date :close}, or nil when the row has no usable price."
  [{:keys [code date close]}]
  (when (and (not (str/blank? (str code)))
             (not (str/blank? (str date)))
             (number? close))
    {:ticker (str/upper-case (str code))
     :date (str date)
     :close (double close)}))

(defn- normalize-rows
  [rows]
  (->> rows
       (keep close-row)
       (sort-by :ticker)
       vec))

(defn latest-session-in
  "Last YYYY-MM-DD for which any ticker has a close."
  [rows]
  (->> rows (keep :date) sort last))

(defn save-prices!
  "Write data/prices-eodhd.edn. :download-date is the newest session in `rows`."
  [rows]
  (dataset/write-dataset! prices-path
                          {:download-date (latest-session-in rows)}
                          rows))

(defn load-prices
  "Rows from data/prices-eodhd.edn."
  []
  (or (dataset/read-rows prices-path)
      (throw (ex-info "Missing prices file; run clj -X:download-eodhd-quotes-market first"
                      {:path prices-path}))))

(defn download-eodhd-quotes-market
  "Download every US symbol's close for the latest session and write
  data/prices-eodhd.edn.

  Usage: clj -X:download-eodhd-quotes-market"
  ([] (download-eodhd-quotes-market {}))
  ([_]
   (println (str "EODHD bulk last-day " exchange "  (entire market)"))
   (flush)
   (let [rows (normalize-rows (fetch-last-day (token)))
         session (latest-session-in rows)]
     (when (empty? rows)
       (throw (ex-info "EODHD bulk last-day returned no usable closes" {})))
     (save-prices! rows)
     (println (format "US closes: %d symbols  session %s" (count rows) session))
     (println (format "Wrote %s" prices-path))
     (println)
     (pprint/print-table [:ticker :date :close] (take 25 rows))
     (println (format "... %d more (see %s)"
                      (max 0 (- (count rows) 25))
                      prices-path))
     {:path prices-path
      :rows (count rows)
      :session session})))
