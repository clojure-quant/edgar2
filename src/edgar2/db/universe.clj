(ns edgar2.db.universe
  "Universe-facts rows for stats (data/universe-facts.edn)."
  (:require [clojure.java.io :as io]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.facts-universe :as facts-universe]))

(def ^:private cache-ttl-ms (* 60 1000))

(defonce ^:private cache* (atom {:expires-at 0 :rows nil}))

(defn load-universe
  "Rows from data/universe-facts.edn. Cached for one minute."
  []
  (let [now (System/currentTimeMillis)
        hit @cache*]
    (if (and (:rows hit) (< now (:expires-at hit)))
      (:rows hit)
      (do
        (when-not (.exists (io/file facts-universe/universe-path))
          (throw (ex-info "Missing universe file; run clj -X:universe-facts first"
                          {:path facts-universe/universe-path})))
        (let [rows (dataset/read-rows facts-universe/universe-path)]
          (reset! cache* {:expires-at (+ now cache-ttl-ms) :rows rows})
          rows)))))

(defn universe-row-by-cik
  "Universe-facts row for `cik`, or nil. Uses `load-universe` (1-minute cache)."
  [cik]
  (let [want (facts-universe/pad-cik cik)]
    (some #(when (= (facts-universe/pad-cik (:cik %)) want) %)
          (load-universe))))
