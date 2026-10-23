(ns edgar2.download.eodhd-exchange
  "EODHD exchange symbol list for US → data/eodhd-exchange.edn.

  One request: GET /api/exchange-symbol-list/US. Each row is :ticker, :name,
  :country, :exchange, :currency, :type, and :isin.

  The token is :eodhd in $QUANTASTORE/edgar2.edn.

  Usage: clj -X:download-eodhd-exchange"
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

(def exchange-path "data/eodhd-exchange.edn")

(def exchange "US")

(def endpoint "https://eodhd.com/api/exchange-symbol-list/")

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

(defn- list-url
  [api-token]
  (str endpoint exchange
       "?api_token=" (URLEncoder/encode api-token StandardCharsets/UTF_8)
       "&fmt=json"))

(defn- public-error
  [s]
  (str/replace (str s) #"api_token=[^&\s\"]+" "api_token=***"))

(defn- without-nil
  "Drop keys whose value is nil, so a missing ISIN is absent rather than :isin nil."
  [m]
  (into (array-map) (remove (comp nil? val) m)))

(defn- normalize-row
  "Lowercase API keys. :Code becomes :ticker."
  [m]
  (without-nil
   (array-map :ticker (:Code m)
              :name (:Name m)
              :country (:Country m)
              :exchange (:Exchange m)
              :currency (:Currency m)
              :type (:Type m)
              :isin (:Isin m))))

(defn- fetch-us
  "Every symbol on the US exchange list."
  [api-token]
  (let [resp (.send (http-client)
                    (-> (HttpRequest/newBuilder)
                        (.uri (URI/create (list-url api-token)))
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
      (throw (ex-info (str "EODHD exchange symbol list HTTP " status)
                      {:status status
                       :body (public-error (subs body 0 (min 500 (count body))))}))

      :else
      (let [parsed (json/read-value body json/keyword-keys-object-mapper)]
        (when-not (sequential? parsed)
          (throw (ex-info "EODHD exchange symbol list was not a list"
                          {:body (public-error (subs body 0 (min 500 (count body))))})))
        (vec parsed)))))

(defn- save-exchange!
  ([rows] (save-exchange! rows (str (LocalDate/now))))
  ([rows download-date]
   (dataset/write-dataset! exchange-path
                           {:download-date download-date
                            :exchange exchange}
                           rows)))

(defn download-eodhd-exchange
  "Download the US exchange symbol list and write data/eodhd-exchange.edn.

  Usage: clj -X:download-eodhd-exchange"
  ([] (download-eodhd-exchange {}))
  ([_]
   (println (format "Downloading EODHD exchange symbol list %s" exchange))
   (flush)
   (let [rows (mapv normalize-row (fetch-us (token)))
         by-type (sort-by val > (frequencies (map :type rows)))]
     (save-exchange! rows)
     (println (format "EODHD exchange %s: %d symbols" exchange (count rows)))
     (println "types" by-type)
     (println (format "Wrote %s" exchange-path))
     (when (seq rows)
       (pprint/print-table [:ticker :name :type :exchange :isin] (take 10 rows)))
     {:path exchange-path
      :exchange exchange
      :rows (count rows)
      :types (into {} by-type)})))
