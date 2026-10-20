(ns edgar2.db.sector
  "OSHA SIC names from resources/sic.edn."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def sic-resource "sic.edn")

(defn- sic-key
  "Four-digit SIC string, or nil."
  [sic]
  (let [s (some-> sic str str/trim not-empty)]
    (when (and s (re-matches #"\d+" s))
      (try (format "%04d" (Long/parseLong s))
           (catch Exception _ nil)))))

(defn- load-sic-index
  []
  (let [url (or (io/resource sic-resource)
                (throw (ex-info "sic.edn is not on the classpath"
                                {:resource sic-resource})))]
    (into {}
          (map (fn [{:keys [sic] :as row}]
                 [sic (select-keys row [:sector :industry :subindustry])]))
          (edn/read-string (slurp url)))))

(def ^:private sic-index
  "Loaded on first use."
  (delay (load-sic-index)))

(defn get-sector-data
  "`:sector`, `:industry`, and `:subindustry` for a SIC code.
  Nil when the code is blank or not in the OSHA manual.
  The manual is read once and kept for the process."
  [sic]
  (when-let [code (sic-key sic)]
    (get @sic-index code)))
