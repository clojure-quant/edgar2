(ns edgar2.filing.xbrl-loader
  "Slurp a filing from a path or URL, then add its statement exhibit when
  the cover links one."
  (:require 
    [clojure.string :as str]))

(defn- statement-exhibit?
  "True for the names an audited-statement exhibit takes beside the cover:
  `_d2.htm`, `ex99d2.htm`, `ex99-2.htm`, `ex-99.2.htm`, `exhibit99-2.htm`,
  and `exhibit992.htm`."
  [name]
  (let [n (str/lower-case (str name))]
    (boolean
     (or (re-find #"[-_]d2\.html?$" n)
         (re-find #"ex(?:hibit)?[-_]?99[-_.]?2\.html?$" n)
         (re-find #"ex99d2\.html?$" n)))))

(defn statement-exhibit-name
  "Filename of the audited-statement exhibit linked from cover HTML.

  Matches a same-filing relative name: `_d2.htm`, `ex99d2.htm`, or an
  `ex99-2` / `exhibit99-2` variant. An absolute archive URL is a citation
  of another filing, not this cover's exhibit. Nil when the cover has no
  such link."
  [html]
  (some (fn [[_ href]]
          (when-not (str/includes? (str href) "/")
            (let [name (str href)]
              (when (statement-exhibit? name)
                name))))
        (re-seq #"(?i)href\s*=\s*\"([^\"]+)\"" (str html))))

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
  when the cover links one and that exhibit can be read. A cover often links
  an exhibit that was never downloaded, so an unreadable sibling is skipped
  rather than thrown. Returns a vector of {:text :where} maps."
  [file-or-url]
  (let [text (slurp file-or-url)
        where (str file-or-url)
        exhibit (statement-exhibit-name text)
        exhibit-where (sibling where exhibit)
        exhibit-text (when exhibit-where
                       (try
                         (slurp exhibit-where)
                         (catch java.io.IOException _ nil)))]
    (cond-> [{:text text :where where}]
      exhibit-text
      (conj {:text exhibit-text :where exhibit-where}))))

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
