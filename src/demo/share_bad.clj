(ns demo.share-bad
  "Tickers with a filing whose share totals lack weighted or outstanding.

  Usage: clj -X:demo-share-bad"
  (:require [edgar2.db.shares :as shares]
            [edgar2.download.dataset :as dataset])
  (:import [java.time LocalDate]))

(def shares-bad-path
  "data/shares-bad2.edn")

(defn- absent?
  "True when a total is missing or zero. `share-info` stores a nil class as 0."
  [n]
  (not (and (number? n) (pos? n))))

(defn bad-rows
  "Share totals that lack :weighted or :outstanding, when :form-type is set."
  []
  (->> (shares/share-tickers)
       sort
       (keep (fn [ticker]
               (let [info (shares/get-shares-ticker ticker)]
                 (when (and (some? (:form-type info))
                            (or (absent? (:weighted info))
                                (absent? (:outstanding info))))
                   {:ticker ticker
                    :cik (:cik info)
                    :filing-date (:filing-date info)
                    :form-type (:form-type info)
                    :weighted (:weighted info)
                    :outstanding (:outstanding info)}))))
       vec))

(defn run
  "Write data/shares-bad2.edn."
  ([] (run {}))
  ([_]
   (let [rows (bad-rows)]
     (dataset/write-dataset! shares-bad-path
                             {:download-date (str (LocalDate/now))}
                             rows)
     (println (format "Wrote %s  (%d tickers)" shares-bad-path (count rows)))
     rows)))
