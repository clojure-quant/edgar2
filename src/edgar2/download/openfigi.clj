(ns edgar2.download.openfigi
  "SEC 13(f) CUSIPs mapped through OpenFIGI exchCode US → data/openfigi.edn.

  OpenFIGI does not return a CUSIP. Each row keeps the CUSIP that was
  queried and the US ticker it returned. Class shares use a hyphen
  (BRK-B), matching company_tickers_exchange.json.

  CALL and PUT lines on the 13(f) list are option contracts and are
  skipped; the underlying stock has its own CUSIP. Set OPENFIGI_APIKEY
  to raise the rate limit (10 jobs and 25 requests/minute without it).

  Usage: clj -X:download-openfigi
         clj -X:download-openfigi :limit 20"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [jsonista.core :as json])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration LocalDate]))

(def openfigi-path "data/openfigi.edn")
(def progress-path "data/openfigi.progress.edn")

(def list-page
  "https://www.sec.gov/rules-regulations/staff-guidance/official-list-section-13f-securities")

(def mapping-url "https://api.openfigi.com/v3/mapping")

(def exch-code "US")

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(defn- api-key
  []
  (some-> (System/getenv "OPENFIGI_APIKEY") str/trim not-empty))

(defn- http-client
  []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.connectTimeout (Duration/ofSeconds 30))
      (.build)))

(defn- response-header
  [resp name]
  (let [v (.firstValue (.headers resp) name)]
    (when (.isPresent v)
      (.get v))))

(defn- http-get
  [client url]
  (let [resp (.send client
                    (-> (HttpRequest/newBuilder)
                        (.uri (URI/create url))
                        (.timeout (Duration/ofSeconds 120))
                        (.header "User-Agent" identity-header)
                        (.header "Accept" "text/plain, text/html, */*")
                        (.GET)
                        .build)
                    (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (when-not (= 200 status)
      (throw (ex-info (str "HTTP " status " for " url)
                      {:url url :status status})))
    (.body resp)))

(defn current-list-url
  "Latest 13(f) .txt link on the SEC list page."
  [html]
  (let [path (second (re-find #"href=\"(/files/investment/13flist[^\"]+\.txt)\""
                              html))]
    (when-not path
      (throw (ex-info "No 13(f) list txt link on the SEC page"
                      {:page list-page})))
    (str "https://www.sec.gov" path)))

(defn- parse-list-line
  "One fixed-width 13(f) row, or nil."
  [line]
  (when (and (>= (count line) 70)
             (re-matches #"[0-9A-Z]{9}" (subs line 0 9)))
    {:cusip (subs line 0 9)
     :description (str/trim (subs line 40 67))
     :status (str/trim (subs line 67 70))}))

(defn read-list
  "Non-option CUSIPs from a 13(f) list file. :option-rows is the CALL/PUT count."
  [text]
  (let [parsed (into [] (keep parse-list-line) (str/split-lines text))
        options (filter #(#{"CALL" "PUT"} (:description %)) parsed)]
    {:cusips (vec (distinct (map :cusip (remove (set options) parsed))))
     :option-rows (count options)}))

(defn- id-type
  "Letter-prefixed 13(f) numbers are CINS; the rest are CUSIPs."
  [cusip]
  (if (Character/isDigit (.charAt ^String cusip 0))
    "ID_CUSIP"
    "ID_CINS"))

(defn- ticker-symbol
  "OpenFIGI writes class shares with a slash (BRK/B)."
  [ticker]
  (some-> ticker str/trim str/upper-case not-empty (str/replace "/" "-")))

(defn- pick-instrument
  "US listing for one mapping result. Common stock wins when several come back."
  [data]
  (let [hits (into []
                   (keep (fn [m]
                           (when (and (= exch-code (:exchCode m))
                                      (ticker-symbol (:ticker m)))
                             (assoc m :ticker (ticker-symbol (:ticker m))))))
                   data)]
    (or (first (filter #(= "Common Stock" (:securityType2 %)) hits))
        (first hits))))

(defn- mapped-rows
  [cusips results]
  (when-not (= (count cusips) (count results))
    (throw (ex-info "OpenFIGI result count does not match the request"
                    {:cusips (count cusips) :results (count results)})))
  (into []
        (keep (fn [[cusip result]]
                (when-let [hit (pick-instrument (:data result))]
                  {:ticker (:ticker hit)
                   :exchange (:exchCode hit)
                   :cusip cusip})))
        (map vector cusips results)))

(defn- pause!
  "Wait out the rate-limit window once this response used the last request."
  [resp]
  (let [remaining (some-> (response-header resp "ratelimit-remaining") parse-long)
        reset (some-> (response-header resp "ratelimit-reset") parse-long)]
    (when (and remaining reset (<= remaining 0) (pos? reset))
      (println (format "  rate limit, waiting %ds" (inc reset)))
      (flush)
      (Thread/sleep (* 1000 (inc reset))))))

(defn- post-mapping!
  [client api-key jobs]
  (loop [attempt 0]
    (let [body (json/write-value-as-string jobs)
          builder (-> (HttpRequest/newBuilder)
                      (.uri (URI/create mapping-url))
                      (.timeout (Duration/ofSeconds 60))
                      (.header "Content-Type" "application/json")
                      (.header "Accept" "application/json"))
          builder (if api-key
                    (.header builder "X-OPENFIGI-APIKEY" api-key)
                    builder)
          resp (.send client
                      (-> builder
                          (.POST (HttpRequest$BodyPublishers/ofString body))
                          .build)
                      (HttpResponse$BodyHandlers/ofString))
          status (.statusCode resp)]
      (cond
        (= 200 status)
        (do (pause! resp)
            (json/read-value (.body resp) json/keyword-keys-object-mapper))

        (and (#{429 500 503} status) (< attempt 6))
        (let [retry (some-> (response-header resp "retry-after") parse-long)
              seconds (or retry (long (Math/pow 2 attempt)))]
          (println (format "  OpenFIGI HTTP %d, retry in %ds" status seconds))
          (flush)
          (Thread/sleep (* 1000 (inc seconds)))
          (recur (inc attempt)))

        :else
        (throw (ex-info (str "OpenFIGI HTTP " status)
                        {:status status
                         :body (let [b (.body resp)]
                                 (subs b 0 (min 500 (count b))))}))))))

(defn- write-edn!
  [path data]
  (let [tmp (str path ".tmp")
        file (io/file path)]
    (some-> (.getParentFile file) .mkdirs)
    (spit tmp (pr-str data))
    (.renameTo (io/file tmp) file)))

(defn- load-progress
  [list-url]
  (let [f (io/file progress-path)]
    (when (.exists f)
      (let [data (edn/read-string (slurp f))]
        (when (= list-url (:list-url data))
          data)))))

(defn- save-rows!
  [rows meta]
  (dataset/write-dataset! openfigi-path meta
                          (sort-by (juxt :ticker :cusip) rows)))

(defn- save-progress!
  [list-url seen rows meta]
  (write-edn! progress-path
              {:list-url list-url
               :seen (vec seen)
               :rows (vec rows)})
  (save-rows! rows meta))

(defn download-openfigi
  "Map the current 13(f) list to US tickers and write data/openfigi.edn.

  `:limit` maps only the first n CUSIPs. A matching progress file is resumed."
  ([] (download-openfigi {}))
  ([{:keys [limit]}]
   (let [client (http-client)
         key (api-key)
         jobs (if key 100 10)
         list-url (current-list-url (http-get client list-page))
         {:keys [cusips option-rows]} (read-list (http-get client list-url))
         cusips (vec (if limit (take (long limit) cusips) cusips))
         progress (load-progress list-url)
         seen (volatile! (set (:seen progress)))
         rows (volatile! (vec (:rows progress)))
         todo (remove @seen cusips)
         meta {:download-date (str (LocalDate/now))
               :exch-code exch-code
               :list-url list-url}
         batches (vec (partition-all jobs todo))
         checkpoint-batches 25]
     (println (format "13(f) %s" list-url))
     (println (format "%d CUSIPs to map (%d already done, %d option rows skipped)"
                      (count cusips) (count @seen) option-rows))
     (println (format "OpenFIGI exchCode %s, %d per request%s"
                      exch-code jobs (if key "" ", no API key")))
     (flush)
     (when (and (empty? todo) (seq rows))
       (save-rows! @rows meta))
     (doseq [[i batch] (map vector (range) batches)]
       (let [results (post-mapping! client key
                                    (mapv (fn [cusip]
                                            {:idType (id-type cusip)
                                             :idValue cusip
                                             :exchCode exch-code})
                                          batch))
             hits (mapped-rows batch results)]
         (vswap! rows into hits)
         (vswap! seen into batch)
         (when (or (zero? (mod (inc i) checkpoint-batches))
                   (= (inc i) (count batches)))
           (save-progress! list-url @seen @rows meta)
           (println (format "  %d/%d queried, %d tickers"
                            (count @seen) (count cusips) (count @rows)))
           (flush))))
     (save-rows! @rows meta)
     (.delete (io/file progress-path))
     (println (format "Wrote %s (%d rows)" openfigi-path (count @rows)))
     (println)
     (pprint/print-table [:ticker :exchange :cusip] (take 15 (sort-by :ticker @rows)))
     @rows)))
