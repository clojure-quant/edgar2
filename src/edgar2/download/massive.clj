(ns edgar2.download.massive
  "Massive ticker overview for every symbol in data/ticker-exchange.edn.

  GET https://api.massive.com/v3/reference/tickers/{ticker}
  Writes data/massive.edn. Symbols already stored there are not requested
  again. The file is rewritten every 30 seconds, so a stopped run resumes.

  This key is limited to 5 requests per minute (a 6th returns HTTP 429).
  Requests are spaced 12.5s apart to stay under that, and a 429 waits
  out the minute. Class shares are requested with Massive's dot (BRK-B
  is asked as BRK.B, then the original symbol if that is missing).

  The API key is :massive in $QUANTASTORE/secrets.edn.

  Usage: clj -X:massive"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.ticker-exchange :as exchanges]
            [jsonista.core :as json])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration LocalDate]))

(def massive-path "data/massive.edn")

(def endpoint "https://api.massive.com/v3/reference/tickers/")

(def requests-per-minute 5)

(def interval-ms
  "Gap between calls. 60s/5 is 12s; the extra 500ms keeps the 6th call
  out of a rolling minute."
  12500)

(def checkpoint-ms 30000)

(def save-lock (Object.))

(defn- api-key []
  (let [root (System/getenv "QUANTASTORE")
        path (io/file root "secrets.edn")]
    (when (or (str/blank? root) (not (.exists path)))
      (throw (ex-info "Missing $QUANTASTORE/secrets.edn (:massive key)"
                      {:path (str path)})))
    (let [value (:massive (edn/read-string (slurp path)))]
      (when (str/blank? (str value))
        (throw (ex-info "No :massive key in secrets.edn" {:path (str path)})))
      (str value))))

(defn- http-client []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.connectTimeout (Duration/ofSeconds 30))
      (.build)))

(defn- encode-symbol
  [s]
  (-> (URLEncoder/encode s StandardCharsets/UTF_8)
      (str/replace "+" "%20")))

(defn- candidate-symbols
  "Massive writes class shares with a dot (BRK.B). Try that, then the
  symbol as it appears in ticker-exchange.edn."
  [ticker]
  (let [dotted (str/replace ticker "-" ".")]
    (if (= dotted ticker) [ticker] [dotted ticker])))

(defn- header
  [resp name]
  (let [v (.firstValue (.headers resp) name)]
    (when (.isPresent v)
      (.get v))))

(defn- pace!
  [last-ms]
  (let [wait (- interval-ms (- (System/currentTimeMillis) @last-ms))]
    (when (pos? wait)
      (Thread/sleep (long wait)))
    (vreset! last-ms (System/currentTimeMillis))))

(defn- parse-results
  [body]
  (:results (json/read-value body json/keyword-keys-object-mapper)))

(defn- request!
  "One ticker GET, paced. Returns the results map, or :missing."
  [client key symbol last-ms]
  (loop [attempt 0]
    (pace! last-ms)
    (let [outcome (try
                    (let [resp (.send client
                                      (-> (HttpRequest/newBuilder)
                                          (.uri (URI/create (str endpoint (encode-symbol symbol))))
                                          (.timeout (Duration/ofSeconds 60))
                                          (.header "Authorization" (str "Bearer " key))
                                          (.header "Accept" "application/json")
                                          (.GET)
                                          .build)
                                      (HttpResponse$BodyHandlers/ofString))]
                      {:status (.statusCode resp)
                       :body (.body resp)
                       :retry-after (some-> (header resp "retry-after") parse-long)})
                    (catch java.io.IOException e
                      {:io e}))]
      (cond
        (:io outcome)
        (if (< attempt 6)
          (let [seconds (long (Math/pow 2 attempt))]
            (println (format "  %s connection failed, waiting %ds" symbol seconds))
            (flush)
            (Thread/sleep (* 1000 seconds))
            (recur (inc attempt)))
          (throw (:io outcome)))

        (= 200 (:status outcome))
        (or (parse-results (:body outcome)) :missing)

        (= 404 (:status outcome))
        :missing

        (= 401 (:status outcome))
        (throw (ex-info "Massive rejected the key in $QUANTASTORE/secrets.edn" {}))

        (and (#{429 500 502 503} (:status outcome)) (< attempt 8))
        (let [seconds (long (or (:retry-after outcome)
                                (if (= 429 (:status outcome))
                                  60
                                  (Math/pow 2 attempt))))]
          (println (format "  HTTP %d for %s, waiting %ds" (:status outcome) symbol seconds))
          (flush)
          (Thread/sleep (* 1000 seconds))
          (recur (inc attempt)))

        :else
        (let [body (str (:body outcome))]
          (throw (ex-info (str "Massive HTTP " (:status outcome) " for " symbol)
                          {:status (:status outcome)
                           :symbol symbol
                           :body (subs body 0 (min 300 (count body)))})))))))

(defn- row-for
  [ticker result]
  (if (map? result)
    (let [api-ticker (:ticker result)]
      (cond-> (assoc result :ticker ticker)
        (and api-ticker (not= api-ticker ticker))
        (assoc :massive-ticker api-ticker)))
    {:ticker ticker :missing true}))

(defn- fetch-ticker!
  [client key ticker last-ms]
  (loop [symbols (candidate-symbols ticker)]
    (let [result (request! client key (first symbols) last-ms)]
      (if (and (= :missing result) (seq (rest symbols)))
        (recur (rest symbols))
        (row-for ticker result)))))

(defn- source-tickers
  []
  (let [rows (dataset/read-rows exchanges/exchanges-path)]
    (when-not rows
      (throw (ex-info "Missing data/ticker-exchange.edn"
                      {:path exchanges/exchanges-path})))
    (into [] (comp (map :ticker) (remove str/blank?) (distinct)) rows)))

(defn- saved-rows
  []
  (vec (or (dataset/read-rows massive-path) [])))

(defn- save!
  [rows]
  (locking save-lock
    (binding [*print-length* nil
              *print-level* nil]
      (dataset/write-dataset! massive-path
                              {:download-date (str (LocalDate/now))}
                              rows))))

(defn- start-checkpoint!
  [stop rows]
  (doto (Thread.
         (fn []
           (try
             (while (not @stop)
               (Thread/sleep checkpoint-ms)
               (when-not @stop
                 (save! @rows)
                 (println (format "wrote %s" massive-path))
                 (flush)))
             (catch InterruptedException _))))
        (.setDaemon true)
        (.setName "massive-checkpoint")
        (.start)))

(defn- report!
  [done total ticker missing?]
  (println (format "%d/%d tickers downloaded %s%s"
                   done total ticker (if missing? " missing" "")))
  (flush))

(defn download
  "Download ticker details into data/massive.edn, resuming from rows already there.

  Usage: clj -X:massive"
  ([] (download {}))
  ([_]
   (let [tickers (source-tickers)
         existing (saved-rows)
         have (set (keep :ticker existing))
         todo (filterv (complement have) tickers)
         total (count tickers)
         done0 (- total (count todo))
         rows (atom existing)
         stop (atom false)
         done (volatile! done0)
         last-ms (volatile! (- (System/currentTimeMillis) interval-ms))
         client (http-client)
         key (api-key)
         minutes (long (Math/ceil (/ (count todo) (/ 60000.0 interval-ms))))
         checkpoint (start-checkpoint! stop rows)
         hook (Thread. (fn [] (save! @rows)) "massive-flush")]
     (.addShutdownHook (Runtime/getRuntime) hook)
     (println (format "%d/%d tickers downloaded" done0 total))
     (println (format "%d left, %d requests/minute, writing %s every 30s"
                      (count todo) requests-per-minute massive-path))
     (when (pos? (count todo))
       (println (format "about %d hours %d minutes remaining"
                        (quot minutes 60) (mod minutes 60))))
     (flush)
     (try
       (when (empty? todo)
         (save! @rows))
       (doseq [ticker todo]
         (let [row (fetch-ticker! client key ticker last-ms)]
           (swap! rows conj row)
           (report! (vswap! done inc) total ticker (:missing row))))
       (finally
         (reset! stop true)
         (.interrupt checkpoint)
         (.join checkpoint 120000)
         (save! @rows)
         (try
           (.removeShutdownHook (Runtime/getRuntime) hook)
           (catch IllegalStateException _))))
     (let [stored @rows
           missing (count (filter :missing stored))]
       (println (format "Wrote %s (%d rows, %d missing)"
                        massive-path (count stored) missing))
       (flush)
       {:path massive-path
        :rows (count stored)
        :missing missing
        :tickers total}))))
