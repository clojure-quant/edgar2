(ns edgar2.util.core
  "SEC identity and the edgarjure disk cache, shared by downloads and demos."
  (:require [edgar.api :as e]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(defn ensure-identity!
  []
  (e/init! identity-header))

(def cache-dir
  "/run/media/florian/26ad683d-44f4-42d8-bbd9-0a5d0b792fe0/edgar-cache")

(defn enable-cache
  []
  (e/enable-disk-cache! :dir cache-dir))
