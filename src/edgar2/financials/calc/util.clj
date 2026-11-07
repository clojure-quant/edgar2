(ns edgar2.financials.calc.util
  (:require
   [tech.v3.dataset :as ds]
   [tablecloth.api :as tc]))

(defn contains-col [ds col]
  (some #(= col %) (tc/column-names ds)))

(defn contains-cols [ds cols]
  (let [a (fn [a b] (and a b))]
    (->> (map #(contains-col ds %) cols)
         (reduce a true))))

(defn approx=
  "True when a and b are within 2% or 1 unit (tag double-counts)."
  [a b]
  (and (number? a) (number? b)
       (let [a (double a)
             b (double b)]
         (<= (Math/abs (- a b))
             (max 1.0 (* 0.02 (Math/abs b)))))))

(defn sum-unique
  "Sum xs, counting near-duplicate tags once."
  [xs]
  (loop [acc [] xs (filter number? xs)]
    (if-let [x (first xs)]
      (if (some #(approx= % x) acc)
        (recur acc (next xs))
        (recur (conj acc (double x)) (next xs)))
      (when (seq acc)
        (reduce + acc)))))

(defn val-or-0 [v]
  (if v v 0.0))

(defn assert-cols [ds cols]
  (assert (contains-cols ds cols) (str "cols needed: " cols)))

(defn add [ds & cols]
  (assert-cols ds cols)
  (map (fn [row]
         (apply + (map #(val-or-0 (get row %)) cols)))
       (ds/mapseq-reader ds)))

(defn subtract [ds & cols]
  (assert-cols ds cols)
  (map (fn [row]
         (apply - (map #(val-or-0 (get row %)) cols)))
       (ds/mapseq-reader ds)))

(defn subtract-when-exist [ds & cols]
  (assert-cols ds cols)
  (map (fn [row]
         (let [vs (map #(get row %) cols)]
           (when-not (some nil? vs)
             (apply - vs))))
       (ds/mapseq-reader ds)))

(defn divide [ds a-col b-col]
  (assert-cols ds [a-col b-col])
  (map (fn [row]
         (let [a (get row a-col)
               b (get row b-col)]
           (when (and a b (> b 0.0))
             (/ a b))))
       (ds/mapseq-reader ds)))