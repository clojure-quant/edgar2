(ns edgar2.screen
  "Run one screen from the registry.

  A screen map has :universe and :calculate. :universe returns the
  tickers to score. :calculate turns that list into rows. The cache
  key is the screen id, its parameters, and the input stamp, so a new
  stats file or a new bar catalog misses. Recalculate recomputes the
  same key.

  Usage:
    clj -X:screen :list true
    clj -X:screen :name cheap-growth :min-mc 1000
    clj -X:screen :name lt-breakout
    clj -X:screen :name ltfsr
    clj -X:screen :name ltfsr :reverse true"
  (:refer-clojure :exclude [run!])
  (:require [clojure.pprint :as pprint]
            [edgar2.screen.registry :as registry])
  (:import [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(defonce cache* (atom {}))

(def ^:private ny (ZoneId/of "America/New_York"))

(def ^:private time-fmt
  (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss z"))

(defn format-time
  [inst]
  (.format time-fmt (.atZone ^Instant inst ny)))

(defn status-line
  "Count, when the rows were calculated, and how long it took."
  [{:keys [n n-universe calculated-at secs]}]
  (format "%d names from %d · calculated %s · %.3f s"
          (long (or n 0))
          (long (or n-universe 0))
          (if calculated-at (format-time calculated-at) "")
          (double (or secs 0))))

(defn resolve-params
  "Parameter values for `screen`, with defaults filled in.

  A param's :parse returns nil for a value it does not accept, and
  that param then stays at its default. false is a value, so a boolean
  option stays false instead of falling back to the default."
  [screen args]
  (into {}
        (map (fn [{:keys [key default parse]}]
               (let [raw (get args key ::missing)]
                 [key (if (= raw ::missing)
                        default
                        (if parse
                          (let [parsed (parse raw)]
                            (if (some? parsed) parsed default))
                          (if (some? raw) raw default)))]))
             (:params screen))))

(defn- input-stamp [screen params]
  (when-let [f (:stamp screen)]
    (f params)))

(defn- cache-key [screen params]
  [(:id screen) params (input-stamp screen params)])

(defn- called-universe [screen params]
  (let [f (:universe screen)]
    (when-not f
      (throw (ex-info "Screen has no universe" {:id (:id screen)})))
    (let [v (f params)]
      (if (and (map? v) (contains? v :items))
        {:items (vec (:items v))
         :lines (vec (or (:lines v) []))}
        {:items (vec v) :lines []}))))

(defn- called-calculate [screen items params]
  (let [f (:calculate screen)]
    (when-not f
      (throw (ex-info "Screen has no calculate" {:id (:id screen)})))
    (let [v (f items params)]
      (if (and (map? v) (contains? v :rows))
        {:rows (vec (:rows v))
         :lines (vec (or (:lines v) []))}
        {:rows (vec v) :lines []}))))

(defn run!
  "Score `screen` and store the rows. Always recomputes."
  [screen args]
  (when-not screen
    (throw (ex-info "No screen" {})))
  (let [params (resolve-params screen args)
        t0 (System/nanoTime)
        {:keys [items lines]} (called-universe screen params)
        calculated (called-calculate screen items params)
        rows (:rows calculated)
        value {:rows rows
               :lines (into lines (:lines calculated))
               :n (count rows)
               :n-universe (count items)
               :calculated-at (Instant/now)
               :secs (/ (double (- (System/nanoTime) t0)) 1.0e9)
               :params params}]
    (swap! cache* assoc (cache-key screen params) value)
    value))

(defn cached-screen
  "Cached rows for `screen`, computing them when the key is missing."
  [screen args]
  (let [params (resolve-params screen args)]
    (or (get @cache* (cache-key screen params))
        (run! screen args))))

(defn recalculate!
  "Recompute the screen named `id`."
  [id args]
  (let [screen (registry/by-id id)]
    (when-not screen
      (throw (ex-info "Unknown screen" {:id id})))
    (run! screen args)))

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
  (println (status-line result))
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
          (print-result screen (run! screen args))
          (unknown! screen-name)))))
  nil)
