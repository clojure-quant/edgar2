(ns edgar2.db.price
  "Session closes from data/prices.edn."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]))

(def prices-path "data/prices-eodhd.edn")

(defn- ticker-key
  [ticker]
  (some-> ticker str str/trim not-empty str/upper-case))

(defn load-prices
  "Rows from data/prices.edn."
  []
  (or (dataset/read-rows prices-path)
      (throw (ex-info "Missing prices file; run clj -X:download-prices first"
                      {:path prices-path}))))

(defn load-price-by-ticker
  "Normalized ticker → close from data/prices.edn."
  []
  (into {}
        (keep (fn [{:keys [ticker close]}]
                (when-let [t (ticker-key ticker)]
                  [t (double close)])))
        (load-prices)))

(def ^:private by-ticker
  "Loaded on first use. Tests rebind this with `with-redefs`."
  (delay (load-price-by-ticker)))

(defn get-price
  "Session close for `ticker`, or nil when the ticker is blank or absent.
  The file is read once and kept for the process."
  [ticker]
  (when-let [t (ticker-key ticker)]
    (get @by-ticker t)))
