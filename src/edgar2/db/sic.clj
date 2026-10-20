(ns edgar2.db.sic
  "SIC enrichment from data/universe-fsds.edn."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.fsds-universe :as fsds]))

(defn load-sic-index
  "Lookups from data/universe-fsds.edn: :by-cik and :by-ticker.
  Each value carries :sic, :sector, :industry, and :subindustry.
  Empty when that file is missing."
  []
  (let [f (io/file fsds/filer-info-path)]
    (if-not (.exists f)
      (do (println (format "  no %s; sic left blank" fsds/filer-info-path))
          {:by-cik {} :by-ticker {}})
      (reduce (fn [idx row]
                (let [text (fn [k]
                             (some-> (get row k) str str/trim not-empty))
                      info (cond-> {}
                             (text :sic) (assoc :sic (text :sic))
                             (text :sector) (assoc :sector (text :sector))
                             (text :industry) (assoc :industry (text :industry))
                             (text :subindustry) (assoc :subindustry (text :subindustry))
                             (text :sic-description) (assoc :sic-description (text :sic-description)))
                      cik (text :cik)
                      ticker (some-> (text :ticker) str/upper-case)]
                  (cond-> idx
                    (and cik (seq info)) (update :by-cik assoc cik info)
                    (and ticker (seq info)) (update :by-ticker assoc ticker info))))
              {:by-cik {} :by-ticker {}}
              (dataset/read-rows f)))))

(defn sic-data
  "SIC fields from universe-fsds for `cik`: `:sic`, `:sector`, `:industry`,
  `:subindustry`, and `:sic-description` when present."
  [cik {:keys [by-cik]}]
  (or (get by-cik (some-> cik str str/trim))
      {}))
