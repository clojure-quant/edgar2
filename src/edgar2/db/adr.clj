(ns edgar2.db.adr
  "ADR ratios from data/adr-list.edn (depositary bank listings)."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]))

(def ^:private adr-list-path "data/adr-list.edn")

(def ^:private ratio-by-ticker
  "ticker → :ratio from data/adr-list.edn. Built on first get-ratio call."
  (delay
    (into {}
          (keep (fn [{:keys [ticker ratio]}]
                  (when (and ticker (number? ratio))
                    [(str/upper-case (str ticker)) ratio])))
          (or (dataset/read-rows adr-list-path) []))))

(defn get-ratio
  "Ordinary shares per one ADR/ADS for `ticker`, or nil when unknown."
  [ticker]
  (when-let [t (some-> ticker str str/trim not-empty str/upper-case)]
    (get @ratio-by-ticker t)))
