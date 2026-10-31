(ns edgar2.db.shares
  "Common shares outstanding from data/share-classes.edn."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.util.ticker-info :as ticker-info]))

(def share-classes-path
  "data/share-classes.edn")

(defn- ticker-key
  [ticker]
  (some-> ticker str str/trim not-empty str/upper-case))

(defn- common-outstanding
  "Value at `[:shares \"Common\" :outstanding]`, or nil."
  [row]
  (get-in row [:shares "Common" :outstanding]))

(defn- index-rows
  "CIK and ticker → common shares outstanding. Rows without that number are skipped."
  [rows]
  {:by-cik (into {}
                 (keep (fn [{:keys [cik] :as row}]
                         (when-let [code (ticker-info/pad-cik cik)]
                           (when-let [n (common-outstanding row)]
                             [code n]))))
                 rows)
   :by-ticker (into {}
                    (keep (fn [{:keys [ticker] :as row}]
                            (when-let [t (ticker-key ticker)]
                              (when-let [n (common-outstanding row)]
                                [t n]))))
                    rows)})

(defn load-shares-index
  "Indexes from data/share-classes.edn. Throws when the file is missing."
  []
  (let [rows (or (dataset/read-rows share-classes-path)
                 (throw (ex-info "Missing data/share-classes.edn"
                                 {:path share-classes-path})))]
    (index-rows rows)))

(def shares-index
  "Loaded on first use. Tests rebind this with `with-redefs`."
  (delay (load-shares-index)))

(defn get-shares-cik
  "Common shares outstanding for `cik`, or nil.
  The file is read once and kept for the process."
  [cik]
  (when-let [code (ticker-info/pad-cik cik)]
    (get (:by-cik @shares-index) code)))

(defn get-shares-ticker
  "Common shares outstanding for `ticker`, or nil.
  The file is read once and kept for the process."
  [ticker]
  (when-let [t (ticker-key ticker)]
    (get (:by-ticker @shares-index) t)))
