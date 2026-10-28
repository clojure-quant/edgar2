(ns edgar2.filing-quarterly-test
  (:require [clojure.test :refer [deftest is testing]]
            [edgar2.filing :as filing]))

(deftest window-months-includes-half-year
  (is (= 3 (filing/window-months {:start "2025-01-01" :end "2025-03-31"})))
  (is (= 6 (filing/window-months {:start "2025-01-01" :end "2025-06-30"})))
  (is (= 9 (filing/window-months {:start "2025-01-01" :end "2025-09-30"})))
  (is (= 12 (filing/window-months {:start "2025-01-01" :end "2025-12-31"}))))

(deftest quarterly-values-from-6k-ytd
  (testing "6-K rows have no :val-q; quarters come from the reported quarter and YTD diffs"
    (let [flow [{:line-item "Revenue" :start "2025-01-01" :end "2025-03-31"
                 :filed "2025-05-20" :form "6-K" :val 100}
                {:line-item "Revenue" :start "2025-01-01" :end "2025-06-30"
                 :filed "2025-08-20" :form "6-K" :val 250}
                {:line-item "Revenue" :start "2025-01-01" :end "2025-09-30"
                 :filed "2025-11-18" :form "6-K" :val 400}]
          annual [{:line-item "Revenue" :start "2025-01-01" :end "2025-12-31"
                   :filed "2026-02-26" :form "20-F" :val 700}]
          values (filing/quarterly-values flow annual "Revenue")]
      (is (= 100 (get values "2025-03-31")))
      (is (= 150.0 (get values "2025-06-30")))
      (is (= 150.0 (get values "2025-09-30")))
      (is (= 300.0 (get values "2025-12-31")))))

  (testing "a reported quarter replaces the YTD difference"
    (let [flow [{:line-item "Revenue" :start "2025-01-01" :end "2025-03-31"
                 :filed "2025-05-20" :form "6-K" :val 100}
                {:line-item "Revenue" :start "2025-04-01" :end "2025-06-30"
                 :filed "2025-08-20" :form "6-K" :val 140}
                {:line-item "Revenue" :start "2025-01-01" :end "2025-06-30"
                 :filed "2025-08-20" :form "6-K" :val 250}]
          values (filing/quarterly-values flow [] "Revenue")]
      (is (= 140 (get values "2025-06-30")))))

  (testing "10-Q :val-q is kept when the row already carries it"
    (let [flow [{:line-item "Revenue" :start "2025-01-01" :end "2025-06-30"
                 :filed "2025-08-01" :form "10-Q" :val 250 :val-q 140}]
          values (filing/quarterly-values flow [] "Revenue")]
      (is (= 140 (get values "2025-06-30"))))))
