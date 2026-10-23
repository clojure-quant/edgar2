(ns edgar2.edgar-util-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.download.edgar-util :as eu]))

(deftest retry-after-reads-seconds-and-http-date
  (let [retry-after-ms @#'eu/retry-after-ms
        soon (+ (System/currentTimeMillis) 30000)
        header (-> (java.time.Instant/ofEpochMilli soon)
                   (.atZone java.time.ZoneOffset/UTC)
                   (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME))]
    (is (= 120000 (retry-after-ms "120")))
    (is (nil? (retry-after-ms "")))
    (is (nil? (retry-after-ms "soon")))
    (let [ms (retry-after-ms header)]
      (is (<= 25000 ms 30000)))))

(deftest slot-line-shows-retry-wait
  (let [line @#'eu/slot-line]
    (is (= "  20260123.nc.tar.gz  136/1189 MB"
           (line "20260123.nc.tar.gz" {:done 136000000 :total 1189000000} 0)))
    (is (= "  20260123.nc.tar.gz  wait 45 (seconds)"
           (line "20260123.nc.tar.gz" {:done 136000000 :total 1189000000 :wait-until 45000}
                 0)))))

(deftest run-queue-counts-and-limits-parallelism
  (let [in-flight (atom 0)
        peak (atom 0)
        stats (eu/run-queue!
               (range 10)
               {:n 3
                :id str
                :work (fn [n]
                        (let [now (swap! in-flight inc)]
                          (swap! peak max now)
                          (Thread/sleep 40)
                          (swap! in-flight dec))
                        (cond
                          (zero? n) (throw (ex-info "boom" {}))
                          (odd? n) :downloaded
                          :else :skipped))
                :fail (fn [n e] {:n n :error (.getMessage e)})})]
    (is (= 5 (:downloaded stats)))
    (is (= 4 (:skipped stats)))
    (is (= [{:n 0 :error "boom"}] (:failed stats)))
    (is (<= @peak 3))))
