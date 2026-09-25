(ns edgar2.facts.download
  (:require [clojure.java.io :as io])
  (:import [java.io FileOutputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def companyfacts-url
  "https://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip")

(def companyfacts-zip-path "data/companyfacts.zip")

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
    (let [total (-> resp .headers (.firstValue "Content-Length") (.orElse "-1") parse-long)
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
