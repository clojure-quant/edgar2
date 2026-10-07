(ns edgar2.web.fsds
  "Ticker lookup for data/universe-fsds.edn (SIC and exchange)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def path "data/universe-fsds.edn")

(def by-ticker
  (delay
    (if (.exists (io/file path))
      (into {}
            (map (fn [row]
                   [(str/upper-case (str (:ticker row))) row]))
            (edn/read-string (slurp path)))
      {})))

(defn company
  "FSDS row for `ticker`, or nil."
  [ticker]
  (get @by-ticker (str/upper-case (str ticker))))
