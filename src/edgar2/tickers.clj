(ns edgar2.tickers
  "SEC company_tickers.json → listed ticker / CIK / name directory."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [jsonista.core :as json]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def tickers-url
  "https://www.sec.gov/files/company_tickers.json")

(def tickers-path "data/tickers.edn")

(defn pad-cik
  [cik]
  (format "%010d" (Long/parseLong (str cik))))

(defn fetch-filers
  "SEC company_tickers.json → seq of {:ticker :cik :name}."
  []
  (let [client (java.net.http.HttpClient/newHttpClient)
        req (-> (java.net.http.HttpRequest/newBuilder)
                (.uri (java.net.URI/create tickers-url))
                (.header "User-Agent" identity-header)
                (.header "Accept" "application/json")
                (.GET)
                (.build))
        resp (.send client req (java.net.http.HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (when-not (= 200 status)
      (throw (ex-info (str "SEC tickers fetch failed: HTTP " status)
                      {:url tickers-url :status status :body (.body resp)})))
    (->> (json/read-value (.body resp) json/keyword-keys-object-mapper)
         vals
         (map (fn [{:keys [ticker cik_str title]}]
                {:ticker ticker
                 :cik (pad-cik cik_str)
                 :name title}))
         (sort-by :ticker))))

(defn ticker-rank
  "Lower rank is closer to common stock (no hyphen, shorter symbol)."
  [ticker]
  [(if (str/includes? (str ticker) "-") 1 0)
   (count (str ticker))
   (str ticker)])

(defn filter-primary-filers
  "Keep the main ticker for each CIK (drop units, warrants, preferred)."
  [filers]
  (->> filers
       (group-by :cik)
       vals
       (map (fn [rows] (first (sort-by (comp ticker-rank :ticker) rows))))
       (sort-by :ticker)))

(defn save-tickers!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str tickers-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file tickers-path))))

(defn load-tickers
  "Rows from data/tickers.edn, or a fresh SEC download if that file is missing."
  []
  (if (.exists (io/file tickers-path))
    (edn/read-string (slurp tickers-path))
    (vec (filter-primary-filers (fetch-filers)))))

(defn download-tickers
  "Download company_tickers.json and write data/tickers.edn
  (one main ticker per CIK).

  Usage: clj -X:download-tickers"
  ([] (download-tickers {}))
  ([_]
   (e/init! identity-header)
   (let [all (vec (fetch-filers))
         rows (vec (filter-primary-filers all))]
     (save-tickers! rows)
     (println (format "SEC listed tickers: %d symbols → %d primary (from %s)"
                      (count all) (count rows) tickers-url))
     (println (format "Wrote %s" tickers-path))
     (println)
     (pprint/print-table [:ticker :cik :name] (take 25 rows))
     (println (format "... %d more (see %s)" (max 0 (- (count rows) 25)) tickers-path))
     rows)))
