(ns edgar2.db.price-change
  "Cached close-change lookups from data/price-change.edn."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]))

(def ^:private path "data/price-change.edn")

(def ^:private change-keys
  [:change50 :change200 :change400])

(defn- changes
  "The three change fields, nil when that lookback is missing."
  [row]
  (into {} (map (fn [k] [k (get row k)]) change-keys)))

(def ^:private by-ticker
  "ticker → {:change50 :change200 :change400}. Built on first get-change call."
  (delay
    (into {}
          (keep (fn [{:keys [ticker] :as row}]
                  (when-let [t (some-> ticker str str/trim not-empty str/upper-case)]
                    [t (changes row)])))
          (or (dataset/read-rows path) []))))

(defn get-change
  "`:change50`, `:change200`, and `:change400` for `ticker`.

  Empty when the ticker is blank or not in data/price-change.edn.
  The file is read once and kept for the process."
  [ticker]
  (if-let [t (some-> ticker str str/trim not-empty str/upper-case)]
    (get @by-ticker t {})
    {}))
