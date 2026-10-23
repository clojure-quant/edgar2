(ns edgar2.download.filing-quarterly-index
  "Quarterly EDGAR master index for one year.

  https://www.sec.gov/Archives/edgar/full-index/{year}/QTR{n}/master.zip

  master.zip is every public filing accepted that quarter (master.idx).
  A quarter the SEC has not published is skipped. A file already on disk
  whose byte length matches the SEC Content-Length is left in place.
  Seven quarters download at once. Every 30 seconds each running file prints
  downloaded-MB/total-MB, or a Retry-After wait in seconds.

  Files land at {dest}/{year}/QTRn/master.zip. dest defaults to data/full-index.

  Usage: clj -X:filing-quarterly-index"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.edgar-util :as eu])
  (:import [java.io FileOutputStream]
           [java.net.http HttpResponse$BodyHandlers]
           [java.nio.file Files StandardCopyOption]))

(def default-year 2026)

(def default-dest "data/full-index")

(def index-name "master.zip")

(defn- master-url
  [year quarter]
  (format "https://www.sec.gov/Archives/edgar/full-index/%d/QTR%d/%s"
          year quarter index-name))

(defn- coerce-year
  [year]
  (let [y (parse-long (str year))]
    (when-not y
      (throw (ex-info "Expected a year" {:year year})))
    y))

(defn- slot-name
  [{:keys [quarter name]}]
  (str "QTR" quarter "/" name))

(defn- local-file
  [dest year quarter name]
  (let [f (io/file dest (str year) (str "QTR" quarter) name)]
    (some-> (.getParentFile f) .mkdirs)
    f))

(defn- part-file
  [dest]
  (io/file (str (.getPath dest) ".part")))

(defn list-indexes
  "Published master.zip files for `year`, Q1 then Q4.
  A quarter that returns 404 is left out."
  ([year] (list-indexes (eu/http-client) year))
  ([client year]
   (vec
    (keep (fn [quarter]
            (let [url (master-url year quarter)
                  len (eu/with-retries #(eu/remote-length client url))]
              (when len
                {:year year
                 :quarter quarter
                 :name index-name
                 :url url
                 :bytes len})))
          [1 2 3 4]))))

(defn- html-body?
  [resp]
  (str/includes? (str/lower-case (str (eu/response-header resp "Content-Type")))
                 "text/html"))

(defn- copy-to-file!
  "Copy `in` to `file`, recording bytes on this download's progress line.
  Returns the number of bytes written."
  [in file total]
  (let [buf (byte-array (* 256 1024))]
    (eu/note-progress! 0 total)
    (with-open [out (FileOutputStream. file)]
      (loop [done 0]
        (let [n (.read in buf)]
          (if (neg? n)
            done
            (let [done' (+ done n)]
              (.write out buf 0 n)
              (eu/note-progress! done' total)
              (recur done'))))))))

(defn- download-file!
  "GET `url` into `dest`. Writes a .part file, then moves it into place
  when the byte count matches Content-Length."
  [client url dest expected]
  (let [part (part-file dest)]
    (when (.exists part)
      (.delete part))
    (let [resp (eu/send! client
                         (eu/request url {:timeout-seconds 180})
                         (HttpResponse$BodyHandlers/ofInputStream))
          status (.statusCode resp)]
      (if (not= 200 status)
        (do (.close (.body resp))
            (eu/throw-status! url status))
        (let [total (or (parse-long (or (eu/response-header resp "Content-Length") ""))
                        expected)]
          (when (html-body? resp)
            (.close (.body resp))
            (throw (ex-info (str "Expected master.zip, got HTML for " url)
                            {:url url :status status :transient true})))
          (let [n (with-open [in (.body resp)]
                    (copy-to-file! in part total))]
            (when (and total (not= (long n) (long total)))
              (throw (ex-info (format "Incomplete download (%s of %s)"
                                      (eu/fmt-bytes n) (eu/fmt-bytes total))
                              {:transient true :have n :expected total :dest (str dest)})))
            (Files/move (.toPath part)
                        (.toPath dest)
                        (into-array [StandardCopyOption/REPLACE_EXISTING]))
            (println (format "  saved %s (%s)" dest (eu/fmt-bytes (.length dest))))
            (flush)
            (.length dest)))))))

(defn- ensure-index!
  "Skip `index` when the local file matches the SEC byte length.
  Otherwise download it. Returns :skipped or :downloaded."
  [client dest {:keys [year quarter name url bytes] :as index}]
  (let [dest-file (local-file dest year quarter name)
        label (slot-name index)]
    (eu/note-progress! (if (.exists dest-file) (.length dest-file) 0) bytes)
    (if (and (.exists dest-file)
             (pos? (.length dest-file))
             (= (.length dest-file) (long bytes)))
      (do (println (format "already downloaded %s (%s)"
                           label (eu/fmt-bytes (.length dest-file))))
          :skipped)
      (do (eu/with-retries #(download-file! client url dest-file bytes))
          :downloaded))))

(defn download
  "Download the quarterly master index for `year` into `dest`.

  One master.zip per published quarter, at
  {dest}/{year}/QTRn/master.zip. Seven quarters download at once.
  A local file whose byte length matches the SEC Content-Length is kept.

  Usage: clj -X:filing-quarterly-index"
  ([] (download {}))
  ([{:keys [year dest] :or {year default-year dest default-dest}}]
   (let [year (coerce-year year)
         dest (str dest)
         _ (or (.isDirectory (io/file dest))
               (.mkdirs (io/file dest))
               (throw (ex-info (str "Could not create destination: " dest)
                               {:dest dest})))]
     (println (format "EDGAR quarterly index %d" year))
     (println (format "  → %s" dest))
     (println (format "  %d at a time, progress every %ds"
                      eu/parallelism (quot eu/progress-ms 1000)))
     (flush)
     (let [client (eu/http-client)
           indexes (list-indexes client year)
           by-q (into {} (map (juxt :quarter identity) indexes))]
       (doseq [q [1 2 3 4]]
         (if-let [row (get by-q q)]
           (println (format "QTR%d: %s (%s)" q index-name (eu/fmt-bytes (:bytes row))))
           (println (format "QTR%d: not published" q))))
       (println (format "%d indexes" (count indexes)))
       (flush)
       (let [{:keys [downloaded skipped failed]}
             (eu/run-queue!
              indexes
              {:id slot-name
               :work #(ensure-index! client dest %)
               :fail (fn [{:keys [quarter name]} e]
                       {:name name :quarter quarter :error (.getMessage e)})})]
         (println (format "Done. %d downloaded, %d already present, %d failed."
                          downloaded skipped (count failed)))
         (flush)
         (when (seq failed)
           (throw (ex-info "Some quarterly indexes failed"
                           {:failed failed :downloaded downloaded :skipped skipped})))
         {:year year
          :dest dest
          :downloaded downloaded
          :skipped skipped
          :failed []})))))
