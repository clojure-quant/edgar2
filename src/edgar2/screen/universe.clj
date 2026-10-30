(ns edgar2.screen.universe
  "Ticker lists a screen can score.

  `stats` reads data/stats.edn and keeps companies at or above :min-mc.
  `stats-all` reads the same file and keeps every company. `eodhd`
  reads the EODHD equity list and keeps symbols that have a bar file.
  None of these functions loads a bar series; that happens in calculate."
  (:require [clojure.java.io :as io]
            [edgar2.download.dataset :as dataset]
            [edgar2.eodhd.ingress :as ingress]
            [edgar2.report.screen :as report]
            [edgar2.report.stats :as stats]))

(def min-mc
  "Market-cap parameter used by the stats screens, in millions of dollars."
  {:key :min-mc
   :label "Min mc ($M)"
   :choices report/mc-choices
   :default report/default-min-mc
   :parse report/parse-min-mc})

(defn stats-stamp
  "Modification time of data/stats.edn. A newer file misses the cache."
  [_]
  [(.lastModified (io/file stats/stats-path))])

(defn- company-items
  [rows]
  (mapv (fn [r]
          {:ticker (:ticker r)
           :name (:entityName r)
           :row r})
        rows))

(defn stats-all
  "Every company in data/stats.edn.

  Each item is {:ticker :name :row}. Market cap is not applied; a
  screen that wants that cut filters in its own pipeline. The
  company count is reported by that pipeline, as the universe stage."
  [_]
  {:items (company-items (report/load-stats))
   :lines []})

(defn stats
  "Companies in data/stats.edn at or above :min-mc.

  Each item is {:ticker :name :row}. A company with no market cap is
  left out at every level, including 0. :lines states the cut."
  [{:keys [min-mc]}]
  (let [min-mc (long (or min-mc report/default-min-mc))
        all (report/load-stats)
        rows (report/at-least-mc all min-mc)]
    {:items (company-items rows)
     :lines [(format "  mc >= %d ($ millions; n=%d of %d)"
                     min-mc (count rows) (count all))]}))

(defn eodhd-stamp
  "Modification times of the equity list and the bar catalog."
  [_]
  [(.lastModified (io/file ingress/equities-path))
   (.lastModified (io/file ingress/bars-meta-path))])

(defn- bar-file? [code]
  (when code
    (.exists (io/file (ingress/bar-file code)))))

(defn eodhd
  "EODHD equities that have a bar file.

  Each item is {:ticker :name :equity}. The series itself is loaded
  later, by calculate. :lines states how many symbols were kept."
  [_]
  (let [equities (or (dataset/read-rows ingress/equities-path) [])
        kept (filterv #(bar-file? (:code %)) equities)]
    {:items (mapv (fn [{:keys [code name] :as equity}]
                    {:ticker code
                     :name (or name "")
                     :equity equity})
                  kept)
     :lines (if (empty? equities)
              ["No equity list yet. Run Bar download on Ingress EODHD."]
              [(format "%d equities, %d with bar files"
                       (count equities) (count kept))])}))
