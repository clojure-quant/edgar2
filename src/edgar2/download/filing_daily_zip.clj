(ns edgar2.download.filing-daily-zip
  "Daily EDGAR dissemination archives, one .nc.tar.gz per filing day.

  https://www.sec.gov/Archives/edgar/Feed/{year}/QTR{n}/{YYYYMMDD}.nc.tar.gz

  Each archive is every public filing disseminated that day. A file already
  on disk whose byte length matches the SEC Content-Length is left in place.
  An interrupted download is stored as a .part file and resumed on the next run.
  Seven days download at once. Every 30 seconds each running day prints
  downloaded-MB/total-MB, or a Retry-After wait in seconds.

  Usage: clj -X:filing-daily-zip"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.edgar-util :as eu])
  (:import [java.io FileOutputStream]
           [java.net.http HttpResponse$BodyHandlers]
           [java.nio.file Files StandardCopyOption]))

(def default-year 2026)

(def default-dest
  "/run/media/florian/26ad683d-44f4-42d8-bbd9-0a5d0b792fe0/sec")

(defn- quarter-url
  [year quarter]
  (format "https://www.sec.gov/Archives/edgar/Feed/%d/QTR%d/" year quarter))

(defn- archive-url
  [year quarter name]
  (str (quarter-url year quarter) name))

(defn index-archives
  "Daily feed archives on a quarter directory page.
  Each map is {:name \"YYYYMMDD.nc.tar.gz\" :size-kb n}."
  [html]
  (->> (re-seq #"<a href=\"(\d{8}\.nc\.tar\.gz)\">.*?</a></td><td>([0-9,]+) KB</td>"
               (str html))
       (keep (fn [[_ name kb]]
               (when-let [n (parse-long (str/replace kb "," ""))]
                 {:name name :size-kb n})))
       (sort-by :name)
       vec))

(defn- fetch-quarter
  "Archives in Feed/{year}/QTRn, or [] when that quarter is not published."
  [client year quarter]
  (let [url (quarter-url year quarter)
        body (eu/with-retries
               (fn []
                 (let [resp (eu/send! client
                                      (eu/request url {:timeout-seconds 60})
                                      (HttpResponse$BodyHandlers/ofString))
                       status (.statusCode resp)]
                   (cond
                     (= 200 status) (.body resp)
                     (= 404 status) ::missing
                     :else (eu/throw-status! url status)))))]
    (if (= body ::missing)
      []
      (index-archives body))))

(defn list-archives
  "Published daily .nc.tar.gz archives for `year`, oldest first."
  ([year] (list-archives (eu/http-client) year))
  ([client year]
   (vec
    (mapcat (fn [quarter]
              (map (fn [row]
                     (assoc row
                            :year year
                            :quarter quarter
                            :url (archive-url year quarter (:name row))))
                   (fetch-quarter client year quarter)))
            [1 2 3 4]))))

(defn- coerce-year
  [year]
  (let [y (parse-long (str year))]
    (when-not y
      (throw (ex-info "Expected a year" {:year year})))
    y))

(defn- local-file
  [dest year quarter name]
  (let [f (io/file dest (str year) (str "QTR" quarter) name)]
    (some-> (.getParentFile f) .mkdirs)
    f))

(defn- part-file
  [dest]
  (io/file (str (.getPath dest) ".part")))

(defn- part-length
  [part]
  (if (.exists part) (.length part) 0))

(defn- content-range-total
  "Byte total from a Content-Range header, or nil."
  [s]
  (when s
    (some-> (re-find #"/(\d+)\s*$" s) second parse-long)))

(defn- content-range-start
  [s]
  (when s
    (some-> (re-find #"bytes\s+(\d+)-" s) second parse-long)))

(defn- response-total
  [resp status have]
  (or (content-range-total (eu/response-header resp "Content-Range"))
      (when-let [len (parse-long (or (eu/response-header resp "Content-Length") ""))]
        (if (= status 206) (+ have len) len))))

(defn- same-length?
  "True when `file` is a finished download of `url`.
  A transient HEAD failure keeps the local file so a finished archive
  is not downloaded again."
  [client url file]
  (let [local (.length file)]
    (try
      (let [remote (eu/remote-length client url)]
        (cond
          (nil? remote)
          (do (println "  not on SEC anymore; keeping local file")
              true)
          (= remote local)
          true
          :else
          (do (println (format "  local %s != SEC %s; downloading again"
                               (eu/fmt-bytes local) (eu/fmt-bytes remote)))
              false)))
      (catch Exception e
        (if (eu/transient-error? e)
          (do (println (format "  could not check SEC size (%s); keeping local file"
                               (.getMessage e)))
              true)
          (throw e))))))

(defn- discard-part!
  [dest]
  (let [part (part-file dest)]
    (when (.exists part)
      (.delete part))))

(defn- copy-counted!
  "Copy `in` to `out`, recording bytes on this download's progress line.
  `done0` bytes are already on disk. Returns the number of bytes written."
  [in out done0 total]
  (let [buf (byte-array (* 1024 1024))]
    (eu/note-progress! done0 total)
    (loop [done (long done0)]
      (let [n (.read in buf)]
        (if (neg? n)
          (- done done0)
          (let [done' (+ done n)]
            (.write out buf 0 n)
            (eu/note-progress! done' total)
            (recur done')))))))

(defn- install-part!
  [part dest total]
  (let [n (.length part)]
    (when-not total
      (throw (ex-info "SEC response had no Content-Length"
                      {:transient true :dest (str dest) :have n})))
    (when (not= n (long total))
      (throw (ex-info (format "Incomplete download (%s of %s)"
                              (eu/fmt-bytes n) (eu/fmt-bytes total))
                      {:transient true :have n :expected total :dest (str dest)})))
    (Files/move (.toPath part)
                (.toPath dest)
                (into-array [StandardCopyOption/REPLACE_EXISTING]))
    (println (format "  saved %s (%s)" dest (eu/fmt-bytes n)))
    (flush)
    n))

(defn- html-body?
  [resp]
  (str/includes? (str/lower-case (str (eu/response-header resp "Content-Type")))
                 "text/html"))

(defn- download-from!
  "GET `url` into `part`, resuming from the bytes already in `part`.
  `restarted?` is true after one full restart, so a bad partial cannot loop.
  Bytes land on the progress line for `id`."
  ([client url dest part id]
   (download-from! client url dest part id false))
  ([client url dest part id restarted?]
   (let [have (part-length part)]
     (when (pos? have)
       (println (format "resuming %s at %s" id (eu/fmt-bytes have))))
     (let [resp (eu/send! client
                          (eu/request url {:range (when (pos? have) have)})
                          (HttpResponse$BodyHandlers/ofInputStream))
           status (.statusCode resp)
           total (response-total resp status have)
           range-start (content-range-start (eu/response-header resp "Content-Range"))]
       (cond
         (and (= 416 status) (pos? have))
         (do (.close (.body resp))
             (cond
               (and total (= (.length part) (long total)))
               (install-part! part dest total)

               restarted?
               (throw (ex-info (str "Range not satisfiable for " url)
                               {:url url :status 416}))

               :else
               (do (println (format "partial %s cannot be resumed; starting over" id))
                   (.delete part)
                   (download-from! client url dest part id true))))

         (and (= 206 status)
              (some? range-start)
              (not= (long range-start) (long have)))
         (do (.close (.body resp))
             (println (format "resume offset for %s was rejected; starting over" id))
             (.delete part)
             (if restarted?
               (throw (ex-info (str "Could not resume " url) {:url url}))
               (download-from! client url dest part id true)))

         (#{200 206} status)
         (with-open [in (.body resp)]
           (when (html-body? resp)
             (throw (ex-info (str "Expected an archive, got HTML for " url)
                             {:url url :status status})))
           (when (and (= 200 status) (pos? have))
             (println (format "server sent the full %s; starting over" id))
             (.delete part))
           (let [done0 (if (= 200 status) 0 have)]
             (with-open [out (FileOutputStream. part (= status 206))]
               (copy-counted! in out done0 total))
             (install-part! part dest total)))

         :else
         (do (.close (.body resp))
             (eu/throw-status! url status)))))))

(defn- part-complete?
  "True when `part` already holds every byte of `url`. Deletes `part` when
  it is longer than the archive."
  [client url part]
  (let [have (.length part)
        remote (eu/remote-length client url)]
    (cond
      (nil? remote) false
      (= have remote) true
      (> have remote)
      (do (println "  partial file is larger than the archive; starting over")
          (.delete part)
          false)
      :else false)))

(defn- download-attempt!
  [client url dest part id]
  (if (and (pos? (part-length part))
           (part-complete? client url part))
    (do (println (format "finishing saved partial %s (%s)" id (eu/fmt-bytes (.length part))))
        (install-part! part dest (.length part)))
    (download-from! client url dest part id)))

(defn- download-archive!
  [client url dest id]
  (let [part (part-file dest)]
    (eu/with-retries
      #(download-attempt! client url dest part id))))

(defn- ensure-archive!
  "Skip `archive` when the local file matches the SEC byte length.
  Otherwise download it. Returns :skipped or :downloaded.
  Runs inside the download queue, which owns the progress line."
  [client dest {:keys [year quarter name url]}]
  (let [dest-file (local-file dest year quarter name)]
    (eu/note-progress! (part-length (part-file dest-file)) nil)
    (if (and (.exists dest-file)
             (pos? (.length dest-file))
             (same-length? client url dest-file))
      (do (discard-part! dest-file)
          (println (format "already downloaded %s (%s)" name (eu/fmt-bytes (.length dest-file))))
          :skipped)
      (do
        (download-archive! client url dest-file name)
        :downloaded))))

(defn- download-all!
  "Download `archives` with seven days in flight."
  [client dest archives]
  (eu/run-queue!
   archives
   {:id :name
    :work #(ensure-archive! client dest %)
    :fail (fn [{:keys [name quarter]} e]
            {:name name :quarter quarter :error (.getMessage e)})}))

(defn download
  "Download every daily EDGAR feed archive for a year into `dest`.

  Files land at {dest}/{year}/QTRn/YYYYMMDD.nc.tar.gz. An archive whose
  local byte length matches the SEC Content-Length is not downloaded again.
  Seven days download at once. Every 30 seconds each running day prints
  downloaded-MB/total-MB, or a Retry-After wait in seconds.

  Usage: clj -X:filing-daily-zip"
  ([] (download {}))
  ([{:keys [year dest] :or {year default-year dest default-dest}}]
   (let [year (coerce-year year)
         dest (str dest)]
     (when-not (.isDirectory (io/file dest))
       (throw (ex-info (str "Destination directory does not exist: " dest)
                       {:dest dest})))
     (println (format "EDGAR daily feed %d" year))
     (println (format "  → %s" dest))
     (println (format "  %d at a time, progress every %ds"
                      eu/parallelism (quot eu/progress-ms 1000)))
     (flush)
     (let [client (eu/http-client)
           archives (list-archives client year)
           n (count archives)
           by-q (frequencies (map :quarter archives))]
       (doseq [q [1 2 3 4]]
         (println (format "QTR%d: %d archives" q (get by-q q 0))))
       (println (format "%d archives" n))
       (flush)
       (let [{:keys [downloaded skipped failed]} (download-all! client dest archives)]
         (println (format "Done. %d downloaded, %d already present, %d failed."
                        downloaded skipped (count failed)))
       (flush)
       (when (seq failed)
         (throw (ex-info "Some daily archives failed"
                         {:failed failed :downloaded downloaded :skipped skipped})))
       {:year year
        :dest dest
        :downloaded downloaded
        :skipped skipped
        :failed []})))))
