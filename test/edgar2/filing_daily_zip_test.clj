(ns edgar2.filing-daily-zip-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.download.filing-daily-zip :as zip]))

(def sample-index
  "<tr><td><a href=\"20260701.nc.tar.gz\"><img class=\"img_icon\" src=\"/icons/text.gif\" alt=\"file icon\">20260701.nc.tar.gz</a></td><td>1296614 KB</td><td>07/01/2026 10:06:28 PM</td></tr>
<tr><td><a href=\"20260702.nc.tar.gz\"><img class=\"img_icon\" src=\"/icons/text.gif\" alt=\"file icon\">20260702.nc.tar.gz</a></td><td>1,821,558 KB</td><td>07/02/2026 10:09:56 PM</td></tr>
<a href=\"../\">Parent Directory</a>
<a href=\"#main-content\">Skip</a>")

(deftest index-archives-reads-daily-feed-rows
  (is (= [{:name "20260701.nc.tar.gz" :size-kb 1296614}
          {:name "20260702.nc.tar.gz" :size-kb 1821558}]
         (zip/index-archives sample-index))))

(deftest index-archives-reads-rows-on-one-line
  (is (= ["20260701.nc.tar.gz" "20260706.nc.tar.gz"]
         (mapv :name
               (zip/index-archives
                (str "<a href=\"20260706.nc.tar.gz\">20260706.nc.tar.gz</a></td><td>10 KB</td>"
                     "<a href=\"20260701.nc.tar.gz\">20260701.nc.tar.gz</a></td><td>20 KB</td>"))))))
