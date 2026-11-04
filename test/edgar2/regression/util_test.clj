(ns edgar2.regression.util-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.util :as util]))

(deftest point-map-drops-rejected-entries-but-keeps-indices
  (is (= [[0.0 1.0] [2.0 3.0]]
         (util/point-map [1 nil 3] #(when (number? %) (double %))))))

(deftest point-map-preserves-order-and-uses-double-indices
  (is (= [[0.0 1.0] [1.0 2.0] [2.0 3.0]]
         (util/point-map [1 2 3] #(when (number? %) (double %))))))
