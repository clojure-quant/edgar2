(ns edgar2.download.edgar-util
  "Shared SEC download pieces: request spacing, HTTP 429 waits,
  the progress lines, and the parallel download queue.

  Seven downloads run at once. Every 30 seconds each one prints
  downloaded-MB/total-MB, or a Retry-After wait in seconds.
  A 429 waits out Retry-After and sends the same request again.
  Request starts are at least 120 ms apart."
  (:require [clojure.string :as str])
  (:import [java.io IOException]
           [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration ZonedDateTime]
           [java.time.format DateTimeFormatter]
           [java.util.concurrent Semaphore]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def parallelism
  "Downloads in flight at once."
  7)

(def progress-ms
  "How often the active downloads print MB/total."
  30000)

(def download-attempts
  "Tries for a transient HTTP or IO failure. A 429 is not one of these;
  send! waits that out on its own."
  4)

(def ^:private pause-ms
  "Gap between SEC request starts. The fair-access cap is 10 requests/second."
  120)

(def ^:private default-retry-ms
  "Wait when a 429 has no Retry-After header."
  60000)

(def ^:private ^:dynamic *progress*
  "Atom of id → progress slot for the download threads, or nil."
  nil)

(def ^:private ^:dynamic *progress-id*
  "Id whose slot *progress* should show a 429 wait."
  nil)

(def ^:private rate-lock (Object.))

(def ^:private next-request-at
  (atom 0))

(defn fmt-bytes
  [n]
  (cond
    (not (number? n)) "?"
    (>= n 1000000000) (format "%.2f GB" (/ (double n) 1.0e9))
    (>= n 1000000) (format "%.0f MB" (/ (double n) 1.0e6))
    :else (format "%d B" (long n))))

(defn- pause!
  "Wait until the next request is allowed. Shared across the parallel downloads."
  []
  (locking rate-lock
    (let [now (System/currentTimeMillis)
          wait (- @next-request-at now)]
      (when (pos? wait)
        (Thread/sleep wait))
      (reset! next-request-at (+ (System/currentTimeMillis) pause-ms)))))

(defn- mb-num
  "Whole megabytes, or \"?\" when the size is unknown."
  [n]
  (if (number? n)
    (str (Math/round (/ (double n) 1.0e6)))
    "?"))

(defn http-client []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.connectTimeout (Duration/ofSeconds 30))
      (.build)))

(defn response-header
  [resp name]
  (let [v (-> resp .headers (.firstValue name))]
    (when (.isPresent v)
      (.get v))))

(defn request
  [url {:keys [head? range timeout-seconds]}]
  (let [b (doto (HttpRequest/newBuilder)
            (.uri (URI/create url))
            (.header "User-Agent" identity-header)
            (.header "Accept" "*/*")
            (.header "Accept-Encoding" "identity"))]
    (when timeout-seconds
      (.timeout b (Duration/ofSeconds (long timeout-seconds))))
    (when range
      (.header b "Range" (str "bytes=" (long range) "-")))
    (if head?
      (.method b "HEAD" (HttpRequest$BodyPublishers/noBody))
      (.GET b))
    (.build b)))

(defn- retry-after-ms
  "Milliseconds until a Retry-After value allows another request.
  The header is either a delay in seconds or an HTTP date. Nil when missing
  or unreadable."
  [s]
  (let [s (str/trim (str s))]
    (cond
      (str/blank? s) nil
      (re-matches #"\d+" s) (* 1000 (parse-long s))
      :else
      (try
        (max 0
             (- (-> (ZonedDateTime/parse s DateTimeFormatter/RFC_1123_DATE_TIME)
                    .toInstant
                    .toEpochMilli)
                (System/currentTimeMillis)))
        (catch Exception _ nil)))))

(defn- close-body!
  [resp]
  (let [body (.body resp)]
    (when (instance? java.io.Closeable body)
      (.close body))))

(defn- mark-wait!
  [until]
  (when (and *progress* *progress-id*)
    (swap! *progress* assoc-in [*progress-id* :wait-until] until)))

(defn- clear-wait!
  []
  (when (and *progress* *progress-id*)
    (swap! *progress*
           (fn [slots]
             (if (get slots *progress-id*)
               (update slots *progress-id* dissoc :wait-until)
               slots)))))

(defn- wait-for-retry!
  "Sleep until Retry-After allows this download to continue.
  The progress slot shows the wait instead of MB/total."
  [resp]
  (let [ms (max 1000 (or (retry-after-ms (response-header resp "Retry-After"))
                         default-retry-ms))
        until (+ (System/currentTimeMillis) ms)]
    (mark-wait! until)
    (try
      (Thread/sleep (long ms))
      (finally
        (clear-wait!)))))

(defn send!
  "Send `req`. On HTTP 429, wait for Retry-After and try the same request again."
  [client req handler]
  (loop []
    (pause!)
    (let [resp (.send client req handler)]
      (if (= 429 (.statusCode resp))
        (do (close-body! resp)
            (wait-for-retry! resp)
            (recur))
        resp))))

(defn- transient-status?
  [status]
  (contains? #{429 500 502 503 504} status))

(defn transient-error?
  [e]
  (or (:transient (ex-data e))
      (instance? IOException e)))

(defn throw-status!
  [url status]
  (throw (ex-info (str "HTTP " status " for " url)
                  {:url url :status status :transient (transient-status? status)})))

(defn remote-length
  "Content-Length of `url`, or nil when the SEC returns 404."
  [client url]
  (let [resp (send! client
                    (request url {:head? true :timeout-seconds 60})
                    (HttpResponse$BodyHandlers/discarding))
        status (.statusCode resp)]
    (cond
      (= 200 status) (parse-long (or (response-header resp "Content-Length") ""))
      (= 404 status) nil
      :else (throw-status! url status))))

(defn with-retries
  "Call `f` up to `n` times. A 429 is handled inside send!.
  Other transient HTTP and IO failures wait and try again."
  ([f] (with-retries download-attempts f))
  ([n f]
   (loop [attempt 1]
     (let [outcome (try [:ok (f)]
                        (catch Exception e
                          [:err e]))]
       (cond
         (= :ok (first outcome)) (second outcome)
         (or (>= attempt n)
             (not (transient-error? (second outcome))))
         (throw (second outcome))
         :else
         (do (println (format "  retry %d/%d after: %s"
                              (inc attempt) n
                              (.getMessage (second outcome))))
             (flush)
             (Thread/sleep (* 2000 attempt))
             (recur (inc attempt))))))))

(defn note-progress!
  "Record bytes for the download running on this thread.
  `total` nil leaves the previous total, or prints as unknown."
  [done total]
  (when (and *progress* *progress-id*)
    (swap! *progress*
           (fn [slots]
             (update slots *progress-id*
                     (fn [slot]
                       (cond-> (assoc (or slot {}) :done done)
                         total (assoc :total total))))))))

(defn- slot-line
  "Progress for one download. A 429 wait replaces MB/total."
  [name {:keys [done total wait-until]} now]
  (if wait-until
    (let [left (max 0 (- (long wait-until) (long now)))
          secs (long (Math/ceil (/ (double left) 1000.0)))]
      (format "  %s  wait %d (seconds)" name secs))
    (format "  %s  %s/%s MB" name (mb-num done) (mb-num total))))

(defn- print-slots!
  "One block: MB/total for each download in flight, or the Retry-After wait."
  [finished total-n slots]
  (let [now (System/currentTimeMillis)
        rows (->> slots (sort-by key))
        lines (map (fn [[name slot]]
                     (slot-line name slot now))
                   rows)]
    (println (str (format "[%d/%d] %d downloading" finished total-n (count rows))
                  "\n"
                  (str/join "\n" lines)
                  "\n"))
    (flush)))

(defn- finished-count
  "Downloaded, skipped, and failed items so far.
  Numeric stats are counts. :failed is a vector of errors."
  [stats]
  (reduce (fn [n [k v]]
            (cond
              (= k :failed) (+ n (count v))
              (number? v) (+ n v)
              :else n))
          0
          stats))

(defn- start-reporter!
  "Print the active downloads every `progress-ms` until `stop` is set."
  [progress stats total-n stop]
  (doto (Thread.
         (fn []
           (try
             (while (not @stop)
               (Thread/sleep (long progress-ms))
               (when-not @stop
                 (print-slots! (finished-count @stats) total-n @progress)))
             (catch InterruptedException _))))
    (.setDaemon true)
    (.setName "edgar-download-progress")
    (.start)))

(defn- item-id
  [id item]
  (if (keyword? id)
    (get item id)
    (id item)))

(defn run-queue!
  "Run `work` on each item with `:n` downloads in flight (default 7).

  `:id` is a keyword or (fn [item] name). That name is the progress line.
  `work` is (fn [item]) and runs with the progress slot bound, so
  note-progress! and a 429 wait land on that line. It returns a keyword
  such as :downloaded or :skipped, which is counted.
  An exception is printed and passed to `:fail`, (fn [item ex] row),
  and stored under :failed.

  Returns {:downloaded n :skipped n :failed [...]} plus any other keyword
  `work` returned."
  [items {:keys [n id work fail]}]
  (when-not (and id work)
    (throw (ex-info "run-queue! needs :id and :work" {:id id :work work})))
  (let [n (or n parallelism)
        fail (or fail (fn [item e]
                        {:id (item-id id item) :error (.getMessage e)}))
        total-n (count items)
        progress (atom {})
        stats (atom {:downloaded 0 :skipped 0 :failed []})
        stop (atom false)
        sem (Semaphore. (int n))
        reporter (start-reporter! progress stats total-n stop)
        futures (mapv (fn [item]
                        (let [slot (item-id id item)]
                          (.acquire sem)
                          (future
                            (try
                              (let [result (binding [*progress* progress
                                                     *progress-id* slot]
                                             (work item))]
                                (when (keyword? result)
                                  (swap! stats update result (fnil inc 0))))
                              (catch Exception e
                                (println (format "FAILED %s %s" slot (.getMessage e)))
                                (flush)
                                (swap! stats update :failed conj (fail item e)))
                              (finally
                                (swap! progress dissoc slot)
                                (.release sem))))))
                      items)]
    (try
      (doseq [f futures] @f)
      (finally
        (reset! stop true)
        (.interrupt reporter)))
    @stats))
