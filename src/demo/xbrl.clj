(ns demo.xbrl
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [edgar2.filing.xbrl :as xbrl]
   [edgar2.filing.xbrl-loader :as xbrl-loader]))




(def x
  (let [docs (xbrl-loader/load-documents "data/filings/10-K/0000317540/0001628280-26-009057/coke-20251231.htm")
        parsed (xbrl/parse-all (map :text docs) {})]
    parsed))

(keys x)
; (:cik :entityName :facts :cover)

(:cik x)
(:entityName x)

(pprint/print-table (:cover x))

(keys (:facts x))
;(:coke :dei :srt :us-gaap)

(count (:facts x))
(pprint/print-table (:facts x))
(pr-str (get-in x [:facts :dei]))

(pprint/print-table (get-in x [:facts :dei]))

