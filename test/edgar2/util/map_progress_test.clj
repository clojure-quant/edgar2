(ns edgar2.util.map-progress-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.util.map-progress :as mp]))

(deftest maps-and-prints-only-the-last-item-of-a-short-run
  (let [calls (atom [])
        out (with-out-str
              (is (= [2 4 6]
                     (mp/map-progress
                      (fn [i total result]
                        (swap! calls conj i)
                        (format "%d/%d -> %s" i total result))
                      #(* 2 %)
                      [1 2 3]))))]
    (is (= [3] @calls))
    (is (= "3/3 -> 6\n" out))))

(deftest reports-every-500-and-the-last-item
  (let [calls (atom [])]
    (is (= (vec (range 501))
           (mp/map-progress
            (fn [i _total _result]
              (swap! calls conj i)
              nil)
            identity
            (range 501))))
    (is (= [500 501] @calls))))

(deftest nil-progress-is-not-printed
  (is (= ""
         (with-out-str
           (mp/map-progress (fn [& _] nil) identity [1])))))

(deftest empty-input-returns-empty-vector
  (is (= []
         (mp/map-progress (fn [& _] "unused") identity []))))

(deftest pmap-keeps-order-and-prints-only-the-last-item
  (let [out (with-out-str
              (is (= [2 4 6]
                     (mp/pmap-progress
                      (fn [i total result]
                        (format "%d/%d -> %s" i total result))
                      #(* 2 %)
                      [1 2 3]))))]
    (is (= "3/3 -> 6\n" out))))

(deftest pmap-reports-every-500-and-the-last-item
  (let [calls (atom [])]
    (is (= (vec (range 501))
           (mp/pmap-progress
            (fn [i _total _]
              (swap! calls conj i)
              nil)
            identity
            (range 501))))
    (is (= [500 501] (sort @calls)))))

(deftest pmap-empty-input-returns-empty-vector
  (is (= []
         (mp/pmap-progress (fn [& _] "unused") identity []))))
