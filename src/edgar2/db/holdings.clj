(ns edgar2.db.holdings
  "13F filers frozen with Nippy and LZ4.

  data/holdings-raw.nippy is one filing per record, including column-7
  shares and the other-manager link. data/holdings.nippy is the pruned
  book the 13F demo loads."
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [taoensso.nippy :as nippy]))

(def path "data/holdings.nippy")
(def raw-path "data/holdings-raw.nippy")

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
  "Freeze pruned `filers` to data/holdings.nippy. Returns the path."
  [filers]
  (write-to path filers))

(defn write-raw
  "Freeze stage-1 `filers` to data/holdings-raw.nippy. Returns the path."
  [filers]
  (write-to raw-path filers))

(defn load
  "Pruned filers previously written by [[write]]."
  []
  (load-from path))

(defn load-raw
  "Stage-1 filers previously written by [[write-raw]]."
  []
  (load-from raw-path))
