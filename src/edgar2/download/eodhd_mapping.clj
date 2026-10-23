(ns edgar2.download.eodhd-mapping
  "EODHD id-mapping for exchange US → data/eodhd-mapping.edn.

  Pages GET /api/id-mapping?filter[ex]=US (1000 rows per page) until every
  mapping is collected. Each row is :ticker, :isin, :figi, :lei, :cusip, and
  :cik. A nil :lei, :cik, or :cusip is left out.

  The token is :eodhd in $QUANTASTORE/edgar2.edn.

  Usage: clj -X:download-eodhd-mapping"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [jsonista.core :as json])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration LocalDate]))

(def mapping-path "data/eodhd-mapping.edn")

(def exchange "US")

(def page-limit 1000)

(def endpoint "https://eodhd.com/api/id-mapping")

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

(defn- page-url
  [api-token offset]
  (str endpoint
       "?filter[ex]=" exchange
       "&page[limit]=" page-limit
       "&page[offset]=" offset
       "&api_token=" (URLEncoder/encode api-token StandardCharsets/UTF_8)
       "&fmt=json"))

(defn- public-error
  [s]
  (str/replace (str s) #"api_token=[^&\s\"]+" "api_token=***"))

(defn- fetch-page
  [client api-token offset]
  (let [resp (.send client
                    (-> (HttpRequest/newBuilder)
                        (.uri (URI/create (page-url api-token offset)))
                        (.timeout (Duration/ofSeconds 120))
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
      (throw (ex-info (str "EODHD id-mapping HTTP " status)
                      {:status status
                       :offset offset
                       :body (public-error (subs body 0 (min 500 (count body))))}))

      :else
      (json/read-value body json/keyword-keys-object-mapper))))

(defn- fetch-us
  "Every id-mapping row for exchange US."
  [api-token]
  (let [client (http-client)]
    (loop [offset 0
           rows []]
      (let [page (fetch-page client api-token offset)
            data (vec (:data page))
            total (:total (:meta page))
            n (count data)
            next-offset (+ offset n)
            rows (into rows data)]
        (println (format "offset %d: %d rows%s"
                         offset
                         n
                         (if total (format ", %d / %d" next-offset total) "")))
        (flush)
        (if (or (zero? n)
                (and total (>= next-offset total))
                (nil? (get-in page [:links :next])))
          rows
          (recur next-offset rows))))))

(def key-order [:ticker :isin :figi :lei :cusip :cik])

(defn- blank?
  [v]
  (or (nil? v) (and (string? v) (str/blank? v))))

(defn- normalize-row
  "Rename :symbol to :ticker. Omit :lei, :cik, and :cusip when nil."
  [m]
  (into (array-map)
        (remove (fn [[k v]]
                  (and (#{:lei :cik :cusip} k) (blank? v))))
        (array-map :ticker (or (:symbol m) (:ticker m))
                   :isin (:isin m)
                   :figi (:figi m)
                   :lei (:lei m)
                   :cusip (:cusip m)
                   :cik (:cik m))))

(defn- print-key-counts
  "How many rows carry each key."
  [rows]
  (let [present (into #{} (mapcat keys) rows)
        ks (concat (filter present key-order)
                   (sort (remove (set key-order) present)))]
    (doseq [k ks]
      (println k (count (filter #(contains? % k) rows))))))

(defn- save-mapping!
  ([rows] (save-mapping! rows (str (LocalDate/now))))
  ([rows download-date]
   (dataset/write-dataset! mapping-path
                           {:download-date download-date
                            :exchange exchange}
                           rows)))

(defn download-eodhd-mapping
  "Download every US id-mapping page and write data/eodhd-mapping.edn.

  Usage: clj -X:download-eodhd-mapping"
  ([] (download-eodhd-mapping {}))
  ([_]
   (let [rows (mapv normalize-row (fetch-us (token)))]
     (save-mapping! rows)
     (println (format "EODHD id-mapping %s: %d rows" exchange (count rows)))
     (println (format "Wrote %s" mapping-path))
     (print-key-counts rows)
     (when (seq rows)
       (pprint/print-table [:ticker :cusip :isin :cik] (take 10 (filter :cusip rows))))
     {:path mapping-path
      :exchange exchange
      :rows (count rows)})))
