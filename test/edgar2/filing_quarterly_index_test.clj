(ns edgar2.filing-quarterly-index-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.download.filing-quarterly-index :as idx]))

(deftest master-url-is-the-quarter-zip
  (is (= "https://www.sec.gov/Archives/edgar/full-index/2026/QTR3/master.zip"
         (@#'idx/master-url 2026 3))))

(deftest slot-name-distinguishes-quarters
  (is (= "QTR3/master.zip"
         (@#'idx/slot-name {:quarter 3 :name "master.zip"}))))
