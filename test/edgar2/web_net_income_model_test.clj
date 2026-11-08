(ns edgar2.web-net-income-model-test
  (:require [clojure.test :refer [deftest is]]
            [tech.v3.dataset :as ds]
            [edgar2.web.net-income-model :as nim]))

(deftest display-value-covers-model-types
  (is (= "" (nim/display-value nil)))
  (is (= "stable" (nim/display-value :stable)))
  (is (= "true" (nim/display-value true)))
  (is (= "1.5" (nim/display-value 1.5)))
  (is (= "Inf" (nim/display-value Double/POSITIVE_INFINITY)))
  (is (= "-Inf" (nim/display-value Double/NEGATIVE_INFINITY)))
  (is (= "NaN" (nim/display-value Double/NaN)))
  (is (= "0.0, 1.0" (nim/display-value [0.0 1.0]))))

(deftest model-rows-sorts-keys-and-formats-values
  (is (= [["intercept" "0.0"]
          ["model" "trend-linear"]
          ["slope" "1.0"]]
         (nim/model-rows {:model :trend-linear :slope 1.0 :intercept 0.0})))
  (is (= [] (nim/model-rows {}))))

(deftest year-rows-oldest-first
  (let [ds (ds/->dataset [{:end "2024-12-31" :revenue 100 :net-income 10}
                          {:end "2023-12-31" :revenue 90 :net-income 9}])]
    (is (= [{:end "2023-12-31" :revenue 90 :net-income 9}
            {:end "2024-12-31" :revenue 100 :net-income 10}]
           (nim/year-rows ds))))
  (is (= [] (nim/year-rows nil))))

(deftest chart-values-long-form-skips-nils
  (let [ds (ds/->dataset [{:end "2024-12-31" :revenue 10 :net-income nil}
                          {:end "2023-12-31" :revenue nil :net-income 5}])]
    (is (= [{:end "2023-12-31" :series "Net Income" :value 5}
            {:end "2024-12-31" :series "Revenue" :value 10}]
           (nim/chart-values ds))))
  (is (= [] (nim/chart-values nil))))

(deftest chart-spec-lines-colors-and-points
  (let [ds (ds/->dataset [{:end "2024-12-31" :revenue 10 :net-income 5}])
        spec (nim/chart-spec ds)
        rev-layer (first (:layer spec))
        ni-group (second (:layer spec))
        bar-layer (first (:layer ni-group))
        est-layer (second (:layer ni-group))
        color-scale {:domain ["Revenue" "Net Income" "EstimateNI"]
                     :range ["#8B4513" "#1f77b4" "#000000"]}]
    (is (= 1200 (:width spec)))
    (is (= 300 (:height spec)))
    (is (= {:scale {:y "independent"}} (:resolve spec)))
    (is (= 2 (count (:layer spec))))
    ;; Revenue layer: line with points on the left Y axis.
    (is (= {:type "line" :point {:size 40}} (:mark rev-layer)))
    (is (= [{:filter "datum.series === 'Revenue'"}] (:transform rev-layer)))
    (is (= "left" (get-in rev-layer [:encoding :y :axis :orient])))
    ;; Both the Revenue layer and the EstimateNI layer carry one full color
    ;; scale so the single legend maps all three series to their colors.
    (is (= color-scale (get-in rev-layer [:encoding :color :scale])))
    (is (= color-scale (get-in est-layer [:encoding :color :scale])))
    ;; Net Income + EstimateNI share the right Y scale via a nested layer group.
    (is (= {:scale {:y "shared"}} (:resolve ni-group)))
    (is (= 2 (count (:layer ni-group))))
    ;; Net Income is a zero-anchored bar, green when positive, red when negative.
    (is (= {:type "bar" :size 10 :opacity 0.75} (:mark bar-layer)))
    (is (= [{:filter "datum.series === 'Net Income'"}] (:transform bar-layer)))
    (is (= {:datum 0} (get-in bar-layer [:encoding :y2])))
    (is (= "right" (get-in bar-layer [:encoding :y :axis :orient])))
    (is (= {:condition {:test "datum.value > 0" :value "#2ca02c"}
            :value "#d62728"}
           (get-in bar-layer [:encoding :fill])))
    ;; EstimateNI stays a line with points on the right Y axis.
    (is (= {:type "line" :point {:size 40}} (:mark est-layer)))
    (is (= [{:filter "datum.series === 'EstimateNI'"}] (:transform est-layer)))
    (is (= "right" (get-in est-layer [:encoding :y :axis :orient])))
    (is (= {:title nil} (get-in est-layer [:encoding :color :legend])))
    (is (= (nim/chart-values ds) (get-in spec [:data :values])))))
