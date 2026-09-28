(ns edgar2.download.fsds
  "Download SEC Financial Statement Data Sets quarter zips."
  (:require [clojure.java.io :as io]
            [edgar.fsds :as dera])
  (:import [java.time LocalDate]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def fsds-dir "data/fsds")

(defn recent-quarters
  "Calendar quarters newest-first, starting at the current quarter."
  [n]
  (let [today (LocalDate/now)]
    (loop [y (.getYear today)
           q (inc (quot (dec (.getMonthValue today)) 3))
           acc []]
      (if (>= (count acc) n)
        acc
        (recur (if (= q 1) (dec y) y)
               (if (= q 1) 4 (dec q))
               (conj acc [y q]))))))

(defn download-fsds-quarter
  [year quarter]
  (let [path (str (io/file fsds-dir (str year "q" quarter ".zip")))]
    (if (.exists (io/file path))
      (do (println (format "  have %dq%d" year quarter))
          path)
      (try
        (println (format "  download %dq%d …" year quarter))
        (flush)
        (dera/download-quarter! year quarter fsds-dir)
        (catch Exception ex
          (println (format "  skip %dq%d (%s)" year quarter (.getMessage ex)))
          nil)))))

(defn ensure-quarters!
  "Newest FSDS zips on disk, downloading as needed.

  Tries up to `:try-n` calendar quarters (default 8) and stops after
  `:keep` successful zips (default 4)."
  ([] (ensure-quarters! {}))
  ([{:keys [keep try-n] :or {keep 4 try-n 8}}]
   (loop [qs (recent-quarters try-n) acc []]
     (cond
       (or (>= (count acc) keep) (empty? qs)) acc
       :else
       (let [path (apply download-fsds-quarter (first qs))]
         (recur (rest qs) (cond-> acc path (conj path))))))))
