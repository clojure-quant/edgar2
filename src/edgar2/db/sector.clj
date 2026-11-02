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

(def ^:private sic-rows*
  "Parsed rows from resources/sic.edn, loaded on first use."
  (delay
    (let [url (or (io/resource sic-resource)
                  (throw (ex-info "sic.edn is not on the classpath"
                                  {:resource sic-resource})))]
      (vec (edn/read-string (slurp url))))))

(defn sic-rows
  "Rows from resources/sic.edn. Each row has `:sector`, `:industry`,
  `:sic`, and `:subindustry`. The resource is read once and kept for
  the process."
  []
  @sic-rows*)

(defn sectors
  "Distinct `:sector` values in `rows`, sorted."
  [rows]
  (vec (sort (distinct (keep :sector rows)))))

(defn industries
  "Distinct `:industry` values inside `sector`, sorted. Empty when
  `sector` is nil."
  [rows sector]
  (if (nil? sector)
    []
    (vec (sort (distinct (keep (fn [row]
                                 (when (= sector (:sector row))
                                   (:industry row)))
                               rows))))))

(defn subindustries
  "Distinct `:subindustry` values inside `sector` and `industry`,
  sorted. Empty unless both parent values are present."
  [rows sector industry]
  (if (or (nil? sector) (nil? industry))
    []
    (vec (sort (distinct (keep (fn [row]
                                 (when (and (= sector (:sector row))
                                            (= industry (:industry row)))
                                   (:subindustry row)))
                               rows))))))

(defn- load-sic-index
  []
  (into {}
        (map (fn [{:keys [sic] :as row}]
               [sic (select-keys row [:sector :industry :subindustry])]))
        (sic-rows)))

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
