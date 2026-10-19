(ns edgar2.download.fx
  "Monthly USD FX rates from Frankfurter → data/fx.edn."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [jsonista.core :as json])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration LocalDate]))

(def fx-path "data/fx.edn")

(def api-base "https://api.frankfurter.dev/v2/rates")

(def quotes
  ["GBP" "EUR" "JPY" "CAD" "AUD" "NZD"
   "BRL" "CNY" "MXN" "ARS" "CHF" "KRW" "TWD" "ILS" "ZAR" "CLP"
   "HKD" "PEN" "COP" "INR" "SGD" "TRY" "DKK" "KZT" "MYR" "PHP" "SEK"])

(def history-years 30)

(defn http-client
  []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.connectTimeout (Duration/ofSeconds 30))
      (.build)))

(defn- enc
  [s]
  (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn rates-url
  "Frankfurter monthly USD→quotes series from `from` through `to` (LocalDate or string)."
  [from to]
  (str api-base
       "?from=" (enc (str from))
       "&to=" (enc (str to))
       "&base=USD"
       "&quotes=" (enc (str/join "," quotes))
       "&group=month"))

(defn fetch-rates
  "Flat Frankfurter observations: {:date :base :quote :rate}."
  [from to]
  (let [url (rates-url from to)
        resp (.send (http-client)
                    (-> (HttpRequest/newBuilder)
                        (.uri (URI/create url))
                        (.timeout (Duration/ofMinutes 2))
                        (.header "Accept" "application/json")
                        (.GET)
                        .build)
                    (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (when-not (= 200 status)
      (throw (ex-info (str "Frankfurter API failed: HTTP " status)
                      {:url url :status status :body (.body resp)})))
    (json/read-value (.body resp) json/keyword-keys-object-mapper)))

(defn pivot-months
  "One row per month: {:date \"YYYY-MM-DD\" :GBP … :EUR …}."
  [observations]
  (->> observations
       (group-by :date)
       (map (fn [[date xs]]
              (into {:date date}
                    (map (fn [{:keys [quote rate]}]
                           [(keyword quote) rate])
                         xs))))
       (sort-by :date)
       vec))

(defn default-from
  []
  (-> (LocalDate/now)
      (.minusYears history-years)
      (.withDayOfMonth 1)))

(defn save-fx!
  ([rows] (save-fx! rows (str (LocalDate/now))))
  ([rows download-date]
   (dataset/write-dataset! fx-path
                           {:download-date download-date
                            :base "USD"
                            :quotes quotes
                            :group "month"}
                           rows)))

(defn load-fx
  "Rows from data/fx.edn, or nil when missing."
  []
  (dataset/read-rows fx-path))

(defonce ^:private rates-cache* (atom {:mtime nil :rates nil}))

(defn- currency-code
  "Uppercase ISO code; strips a trailing unit suffix like `/shares`."
  [currency]
  (when currency
    (let [s (-> currency str str/trim str/upper-case)]
      (when (seq s)
        (or (second (re-matches #"([A-Z]{3})(?:/.*)?" s))
            s)))))

(defn latest-rates
  "Most recent month from data/fx.edn as `{currency-kw rate}`, excluding `:date`.
  Cached by file mtime. Nil when the file is missing or empty."
  []
  (let [f (io/file fx-path)]
    (when (.exists f)
      (let [mtime (.lastModified f)
            hit @rates-cache*]
        (if (and (:rates hit) (= mtime (:mtime hit)))
          (:rates hit)
          (let [rates (some-> (load-fx)
                              last
                              (dissoc :date)
                              not-empty)]
            (reset! rates-cache* {:mtime mtime :rates rates})
            rates))))))

(defn from-usd
  "Units of `currency` per 1 USD from the latest fx.edn row.
  USD is 1.0. Nil when the currency is unknown."
  [currency]
  (let [code (currency-code currency)]
    (cond
      (nil? code) nil
      (= code "USD") 1.0
      :else (when-let [r (get (latest-rates) (keyword code))]
              (double r)))))

(defn to-usd
  "USD per 1 unit of `currency` from the latest fx.edn row.
  USD is 1.0. Nil when the currency is unknown."
  [currency]
  (when-let [r (from-usd currency)]
    (when-not (zero? r)
      (/ 1.0 r))))

(defn download-fx
  "Pull ~30 years of monthly USD rates and write data/fx.edn.

  Usage: clj -X:download-fx"
  ([] (download-fx {}))
  ([{:keys [from to]}]
   (let [to (or (some-> to str) (str (LocalDate/now)))
         from (or (some-> from str) (str (default-from)))
         raw (fetch-rates from to)
         rows (pivot-months raw)]
     (save-fx! rows)
     (println (format "Frankfurter monthly FX: %d months × %s (USD base) from %s"
                      (count rows) (str/join "," quotes) api-base))
     (println (format "range %s → %s" from to))
     (when (seq rows)
       (println "first" (first rows))
       (println "last " (last rows)))
     (println (format "Wrote %s" fx-path))
     rows)))
