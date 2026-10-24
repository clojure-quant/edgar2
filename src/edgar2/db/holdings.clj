(ns edgar2.db.holdings
  "13F filers frozen with Nippy and LZ4.

  data/holdings-raw-<year>.nippy is one filing per record, including
  column-7 shares and the other-manager link. data/holdings-<year>.nippy
  is the pruned book the 13F demo loads."
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [taoensso.nippy :as nippy]))

(defn path
  "Pruned holdings for `year`, data/holdings-<year>.nippy."
  [year]
  (str "data/holdings-" year ".nippy"))

(defn raw-path
  "Stage-1 holdings for `year`, data/holdings-raw-<year>.nippy."
  [year]
  (str "data/holdings-raw-" year ".nippy"))

(defn- write-to
  [dest filers]
  (let [file (io/file dest)
        tmp (io/file (str dest ".tmp"))]
    (some-> (.getParentFile file) .mkdirs)
    (nippy/freeze-to-file tmp {:filers (vec filers)})
    (.renameTo tmp file)
    dest))

(defn- load-from
  [dest]
  (:filers (nippy/thaw-from-file (io/file dest))))

(defn write
  "Freeze pruned `filers` to data/holdings-<year>.nippy. Returns the path."
  [year filers]
  (write-to (path year) filers))

(defn write-raw
  "Freeze stage-1 `filers` to data/holdings-raw-<year>.nippy. Returns the path."
  [year filers]
  (write-to (raw-path year) filers))

(defn load
  "Pruned filers previously written by [[write]] for `year`."
  [year]
  (load-from (path year)))

(defn load-raw
  "Stage-1 filers previously written by [[write-raw]] for `year`."
  [year]
  (load-from (raw-path year)))
