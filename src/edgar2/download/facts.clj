(ns edgar2.download.facts
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io FileOutputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Instant LocalDate ZoneId ZonedDateTime]
           [java.time.format DateTimeFormatter]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def companyfacts-url
  "https://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip")

(def companyfacts-zip-path "data/companyfacts.zip")

(def companyfacts-date-path
  "YYYY-MM-DD from the zip's Last-Modified header, written next to the zip."
  "data/companyfacts.date")

(defn http-date->iso
  "Calendar date (UTC) from an HTTP Last-Modified value, or nil."
  [s]
  (when-not (str/blank? (str s))
    (try
      (str (.toLocalDate (ZonedDateTime/parse s DateTimeFormatter/RFC_1123_DATE_TIME)))
      (catch Exception _ nil))))

(defn file-day
  "Local calendar day for a file's last-modified time."
  [file]
  (str (LocalDate/ofInstant (Instant/ofEpochMilli (.lastModified file))
                            (ZoneId/systemDefault))))

(defn remember-date!
  [date]
  (when-not (str/blank? (str date))
    (.mkdirs (io/file "data"))
    (spit companyfacts-date-path (str date))
    (str date)))

(defn companyfacts-date
  "Publication day of data/companyfacts.zip.

  A fresh download stores the SEC Last-Modified day. An older zip with no
  saved header uses the file's own last-modified day."
  []
  (let [sidecar (io/file companyfacts-date-path)
        zip (io/file companyfacts-zip-path)]
    (cond
      (.exists sidecar) (not-empty (str/trim (slurp sidecar)))
      (.exists zip) (remember-date! (file-day zip))
      :else nil)))

(defn fmt-mb
  [bytes]
  (format "%.0f MB" (/ (double bytes) 1.0e6)))

(defn download-with-progress
  "Stream `url` to `dest`, printing size and percent as bytes arrive."
  [url dest]
  (.mkdirs (.getParentFile (io/file dest)))
  (let [tmp (str dest ".part")
        client (-> (HttpClient/newBuilder)
                   (.followRedirects java.net.http.HttpClient$Redirect/NORMAL)
                   (.build))
        req (-> (HttpRequest/newBuilder)
                (.uri (URI/create url))
                (.header "User-Agent" identity-header)
                (.GET)
                (.build))
        resp (.send client req (HttpResponse$BodyHandlers/ofInputStream))
        status (.statusCode resp)]
    (when-not (= 200 status)
      (throw (ex-info (str "Download failed: HTTP " status) {:url url :status status})))
    (let [published (or (http-date->iso (-> resp .headers (.firstValue "Last-Modified") (.orElse "")))
                        (str (LocalDate/now)))
          total (-> resp .headers (.firstValue "Content-Length") (.orElse "-1") parse-long)
          step (max (* 8 1024 1024) (quot (max total 1) 20))]
      (println (format "Downloading %s" url))
      (println (format "  → %s  (%s)" dest (if (pos? total) (fmt-mb total) "size unknown")))
      (flush)
      (with-open [in (.body resp)
                  out (FileOutputStream. tmp)]
        (let [buf (byte-array 65536)]
          (loop [done 0 printed 0]
            (let [n (.read in buf)]
              (if (neg? n)
                (do (println (format "  %s / %s  (100%%)"
                                     (fmt-mb done)
                                     (if (pos? total) (fmt-mb total) (fmt-mb done))))
                    (flush)
                    done)
                (do (.write out buf 0 n)
                    (let [done' (+ done n)]
                      (if (>= (- done' printed) step)
                        (do (println (format "  %s / %s  (%.0f%%)"
                                             (fmt-mb done')
                                             (if (pos? total) (fmt-mb total) "?")
                                             (if (pos? total)
                                               (* 100.0 (/ done' total))
                                               0.0)))
                            (flush)
                            (recur done' done'))
                        (recur done' printed)))))))))
      (.renameTo (io/file tmp) (io/file dest))
      (remember-date! published)
      (println (format "  published %s" published))
      dest)))

(defn ensure-zip!
  "Reuse data/companyfacts.zip unless :force is true."
  ([] (ensure-zip! {}))
  ([{:keys [force]}]
   (let [zip (io/file companyfacts-zip-path)]
     (if (and (.exists zip) (not force))
       (println (format "Using cached %s  (%s)"
                        companyfacts-zip-path
                        (fmt-mb (.length zip))))
       (download-with-progress companyfacts-url companyfacts-zip-path))
     companyfacts-zip-path)))
