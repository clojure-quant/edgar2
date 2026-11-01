(ns edgar2.filing.xbrl-loader
  "Slurp a filing from a path or URL, then add its statement exhibit when
  the cover links one."
  (:require 
    [clojure.string :as str]
    [edgar2.filing.xbrl :as xbrl]))

(defn- sibling
  "Same directory as `where`, with `name` as the filename."
  [where name]
  (when (and where name)
    (let [s (str where)
          i (str/last-index-of s "/")]
      (when i
        (str (subs s 0 (inc i)) name)))))

(defn load-documents
  "Slurp `file-or-url` (a path or a URL), then add the statement exhibit
  when the cover links one. Returns a vector of {:text :where} maps."
  [file-or-url]
  (let [text (slurp file-or-url)
        where (str file-or-url)
        exhibit (xbrl/statement-exhibit-name text)
        exhibit-where (sibling where exhibit)]
    (cond-> [{:text text :where where}]
      exhibit-where
      (conj {:text (slurp exhibit-where) :where exhibit-where}))))

(comment
  ;; Test in the REPL (send to nREPL via Calva):
  ;;
  ;;   clj -M
  (require '[edgar2.filing.xbrl-loader :as xbrl-loader])
  ;; pick a random folder from data/filings/10-K
  (xbrl-loader/load-documents "data/filings/10-K/0000317540/0001628280-26-009057/coke-20251231.htm")
  ;; =>
  ;; [{:text "...", :where "data/filings/10-K/0000317540/0001628280-26-009057/coke-20251231.htm"}]
  )
