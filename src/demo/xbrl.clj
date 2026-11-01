(ns demo.xbrl
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [edgar2.filing.xbrl-loader :as xbrl-loader]
   [edgar2.filing.xbrl :as xbrl]))

(defn- printable-context
  [row]
  (let [{:keys [period entity dimension]} row]
    {:context-id (:context-id row)
     :period (if (and (:start period) (:end period))
               (str (:start period) "-" (:end period))
               (:instant period))
     :identifier (:identifier entity)
     :dimension (str/join ", " (map str (vals dimension)))}))

(defn print-context
  "Print context rows as a table: a duration as a start-end range, the
  entity identifier, and the dimension members without their axes."
  [contexts]
  (pprint/print-table (map printable-context contexts)))


(def docs (xbrl-loader/load-documents "http://localhost:3000/filing/0000764180/0000764180-26-000114/mo-20260930.htm#i7a3e4ca43ebf4b05b52a94c0195b3334_145"))



(def docs (xbrl-loader/load-documents "data/filings/10-K/0000317540/0001628280-26-009057/coke-20251231.htm"))

(def x (xbrl/parse-all (map :text docs) {}))



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

;; summary
(count docs)
(-> docs first keys)
(-> docs first :where)

(def summary (xbrl/overview docs))

(keys summary)

(:facts summary)

(-> (:facts summary) first keys)

(pprint/print-table (->> (:facts summary)
                         (filter #(= :dei (:taxonomy %)))
                         (remove #(= :DocumentsIncorporatedByReferenceTextBlock  (:concept %)))
                         ;(take 30)
                         (map #(select-keys % [:taxonomy :concept :value]))
                     ))
(count (:facts summary))

(->> (:context summary)
     (take 50)
     (print-context))



(count (:context summary))
(count (:dimension summary))

(pprint/print-table (take 10 (:dimension summary)))



summary