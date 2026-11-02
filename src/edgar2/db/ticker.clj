(ns edgar2.db.ticker
  "Ticker and CIK lookups from data/ticker-exchange.edn."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.util.ticker-info :as ticker-info]))

(def exchange-rows-path "data/ticker-exchange.edn")

(defn- ticker-key
  [ticker]
  (some-> ticker str str/trim not-empty str/upper-case))

(defn load-ticker-index
  "Indexes from data/ticker-exchange.edn.
  :rows-by-cik groups the raw CIK in file (SEC) order.
  :cik-by-ticker and :name-by-ticker are keyed by upper-case ticker."
  ([] (load-ticker-index (or (dataset/read-rows exchange-rows-path) [])))
  ([rows]
   (let [rows (vec rows)]
     {:rows-by-cik (group-by :cik rows)
      :cik-by-ticker
      (into {}
            (keep (fn [{:keys [ticker cik]}]
                    (when-let [t (ticker-key ticker)]
                      (when-let [code (ticker-info/pad-cik cik)]
                        [t code]))))
            rows)
      :name-by-ticker
      (into {}
            (keep (fn [{:keys [ticker name]}]
                    (when-let [t (ticker-key ticker)]
                      (when-let [n (some-> name str str/trim not-empty)]
                        [t n]))))
            rows)})))

(def ticker-index
  "Loaded on first use. Tests rebind this with `with-redefs`."
  (delay (load-ticker-index)))

(defn ticker->cik
  "10-digit CIK for a listed ticker, or nil."
  [ticker]
  (when-let [t (ticker-key ticker)]
    (get (:cik-by-ticker @ticker-index) t)))

(defn ticker->name
  "Registrant name for a listed ticker, or nil."
  [ticker]
  (when-let [t (ticker-key ticker)]
    (get (:name-by-ticker @ticker-index) t)))

(defn- listed-rows
  "Exchange rows for `cik` in file order (primary first)."
  [cik]
  (if-let [padded (ticker-info/pad-cik cik)]
    (->> (get (:rows-by-cik @ticker-index) padded)
         (filterv #(not (str/blank? (str (:ticker %))))))
    []))

(defn tickers-for-cik
  "Every listed ticker for `cik`, primary first (SEC order). Empty when absent."
  [cik]
  (vec (distinct (map :ticker (listed-rows cik)))))

(defn primary-ticker-for-cik
  "Primary listed ticker and exchange for `cik`, or nil.
  Primary is the first symbol for that CIK in company_tickers_exchange.json
  (same choice `filter-primary-filers` makes)."
  [cik]
  (when-let [row (first (listed-rows cik))]
    {:ticker (:ticker row)
     :exchange (:exchange row)}))
