(ns edgar2.util.core
  "SEC identity and the edgarjure disk cache, shared by downloads and demos."
  (:require [edgar.api :as e]
            [edgar2.config :as config]
            ))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(defn ensure-identity!
  []
  (e/init! identity-header))



(defn enable-cache
  []
  (e/enable-disk-cache! :dir config/cache-dir))
