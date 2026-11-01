(ns edgar2.cli.screen
  "Run one screen from the registry.

  Usage:
    clj -X:screen :list true
    clj -X:screen :name cheap-growth :min-mc 1000
    clj -X:screen :name lt-breakout
    clj -X:screen :name ltfsr
    clj -X:screen :name ltfsr :reverse true"
  (:require [clojure.pprint :as pprint]
            [edgar2.screen.core :as core]
            [edgar2.screen.registry :as registry]))

(defn- as-id [screen-name]
  (cond
    (keyword? screen-name) screen-name
    (symbol? screen-name) (keyword screen-name)
    (string? screen-name) (keyword screen-name)
    :else nil))

(defn- print-catalog []
  (println "Screens:")
  (doseq [screen (registry/all)]
    (println (format "  %-28s %s" (name (:id screen)) (:title screen))))
  (println)
  (println "clj -X:screen :name cheap-growth :min-mc 1000")
  (println "clj -X:screen :name lt-breakout")
  (println "clj -X:screen :list true"))

(defn- print-result [screen result]
  (println (:title screen))
  (doseq [line (:description screen)]
    (println line))
  (doseq [line (:lines result)]
    (println line))
  (println (core/status-line result))
  (when (seq (:rows result))
    (println)
    (pprint/print-table (mapv :key (:columns screen)) (:rows result))))

(defn- unknown! [screen-name]
  (println (str "Unknown screen " screen-name))
  (print-catalog)
  (throw (ex-info (str "Unknown screen " screen-name)
                  {:name screen-name
                   :screens (mapv :id (registry/all))})))

(defn run
  "Print one screen, or the catalog.

  `:list true` prints the ids. `:name` is the screen id, as a keyword,
  a symbol, or a string. Other keys are that screen's parameters."
  [args]
  (let [args (or args {})
        screen-name (:name args)]
    (cond
      (or (:list args) (nil? screen-name))
      (print-catalog)

      :else
      (let [screen (registry/by-id (as-id screen-name))]
        (if screen
          (print-result screen (core/run! screen args))
          (unknown! screen-name)))))
  nil)
