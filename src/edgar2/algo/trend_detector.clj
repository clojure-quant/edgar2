(ns edgar2.algo.trend-detector
  "Net-income model classification from data/ni-trend.edn, written by
  demo.net-income-model. Shows ticker, model, and r-squared."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.screen.util :as util]))

(def path "data/ni-trend.edn")

(defn- parse-sort
  "One of :ticker or :model. Accepts the keyword or its string, with or
  without a leading colon (the web select sends the string)."
  [x]
  (cond
    (= :ticker x) :ticker
    (= :model x) :model
    (string? x) (case (str/replace (str/trim x) #"^:" "")
                  "ticker" :ticker
                  "model" :model
                  nil)
    :else nil))

(def sort-param
  {:key :sort
   :label "Sort"
   :choices [:ticker :model]
   :default :ticker
   :parse parse-sort})

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :model :label "Model"}
   {:key :r-squared :label "r-squared" :num? true :decimals 3}])

(defn- round3 [x]
  (when (number? x) (/ (Math/round (* (double x) 1000.0)) 1000.0)))

(defn universe
  "Rows from data/ni-trend.edn. Empty with an explanatory line when the
  file is missing."
  [_]
  (let [rows (dataset/read-rows path)]
    (if (seq rows)
      {:items rows
       :lines [(format "%d rows from %s" (count rows) path)]}
      {:items []
       :lines [(format "No rows in %s; run clj -X:demo-net-income-model" path)]})))

(defn- row [r]
  {:ticker (:ticker r)
   :model (some-> (:model r) name)
   :r-squared (round3 (:r-squared r))})

(defn- r-squared-of [r]
  (or (:r-squared r) Double/NEGATIVE_INFINITY))

(defn- model-compare [a b]
  (let [c (compare (:model a) (:model b))]
    (if (zero? c)
      (compare (r-squared-of b) (r-squared-of a))
      c)))

(defn calculate
  "Rows in the requested order. `:sort` is the sort parameter."
  [items params]
  (let [sort-key (util/param-value params sort-param)
        rows (mapv row items)
        sorted (if (= :model sort-key)
                 (vec (sort model-compare rows))
                 (vec (sort-by :ticker rows)))
        line (if (= :model sort-key)
               "sorted by model, then r-squared descending"
               "sorted by ticker")]
    {:rows sorted
     :lines [line]}))

(defn stamp
  "Modification time of data/ni-trend.edn."
  [_]
  [(.lastModified (io/file path))])

(def screen
  {:id :trend-detector
   :title "Trend detector"
   :description ["Net-income model per ticker from data/ni-trend.edn."
                 "Sort by ticker, or by model with r-squared descending."]
   :params [sort-param]
   :columns columns
   :universe universe
   :calculate calculate
   :stamp stamp})
