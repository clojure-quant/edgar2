(ns edgar2.screen.util
  "Filter pipelines and parameters shared by the screens.

  apply-and-pipeline walks {:label :pred} steps in order. Each
  predicate runs on the tickers that survived the previous step.
  The first stage is the incoming count, labeled universe.
  report-pipeline-info formats those stages as the count lines the
  screen prints, label on the left and survivors on the right.

  choice-param builds a dropdown parameter from a fixed choice list;
  param-value resolves a typed value with the param's default as the
  fallback."
  (:require [clojure.string :as str]))

(defn as-number
  "Number value of `x`, or nil. Accepts a number or a decimal string."
  [x]
  (cond
    (number? x) (double x)
    (string? x) (try (Double/parseDouble (str/trim x))
                     (catch Exception _ nil))
    :else nil))

(defn parse-choice
  "One of `choices`, or nil. Accepts the number or its decimal string.
  Choices are matched numerically and the original choice is returned."
  [choices x]
  (let [n (as-number x)]
    (when (some? n)
      (some (fn [choice]
              (when (and (number? choice)
                         (= n (double choice)))
                choice))
            choices))))

(defn choice-param
  "A parameter whose value is one of `choices`."
  [key label choices default]
  {:key key
   :label label
   :choices choices
   :default default
   :parse (fn [x] (parse-choice choices x))})

(defn param-value
  "Value of `param` in `params`, or that param's default."
  [params {:keys [key default]}]
  (let [v (get params key)]
    (if (some? v) v default)))

(defn at-least
  "Predicate: `k` is a number and is at least `minimum`."
  [k minimum]
  (fn [row]
    (let [v (get row k)]
      (and (number? v)
           (>= (double v) (double minimum))))))

(defn between
  "Predicate: `k` is a number and is between `lo` and `hi`, inclusive."
  [k lo hi]
  (fn [row]
    (let [v (get row k)]
      (and (number? v)
           (<= (double lo) (double v) (double hi))))))

(defn at-most
  "Predicate: `k` is a number and is at most `maximum`."
  [k maximum]
  (fn [row]
    (let [v (get row k)]
      (and (number? v)
           (<= (double v) (double maximum))))))

(defn number-at
  "Predicate: the value at `k` is a number."
  [k]
  (fn [row]
    (number? (get row k))))

(defn positive-at
  "Predicate: the value at `k` is a number and is positive."
  [k]
  (fn [row]
    (let [v (get row k)]
      (and (number? v)
           (pos? (double v))))))

(defn apply-and-pipeline
  "Filter `tickers` through `filter-ands` in order.

  Each step is {:label :pred}. :pred is applied to the tickers still
  in the list. Returns {:tickers <survivors> :stages [{:label :n} ...]}.
  The first stage is the incoming count, labeled universe. Each later
  :n is how many tickers remain after that step."
  [tickers filter-ands]
  (let [tickers (vec tickers)]
    (reduce (fn [{:keys [tickers] :as result} {:keys [label pred]}]
              (when-not (ifn? pred)
                (throw (ex-info "Filter step needs a :pred function"
                                {:label label})))
              (let [kept (filterv pred tickers)]
                {:tickers kept
                 :stages (conj (:stages result)
                               {:label (str label) :n (count kept)})}))
            {:tickers tickers
             :stages [{:label "universe" :n (count tickers)}]}
            filter-ands)))

(defn report-pipeline-info
  "Lines for `pipeline-result`: each stage label, and the count still
  in the list. Counts share a column on the right."
  [pipeline-result]
  (let [stages (vec (:stages pipeline-result))
        widest (reduce max 0 (map #(count (:label %)) stages))
        label-width (max 64 (+ widest 2))]
    (mapv (fn [{:keys [label n]}]
            (format (str "%-" label-width "s%6d") label (long n)))
          stages)))
