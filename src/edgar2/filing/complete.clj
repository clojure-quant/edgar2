(ns edgar2.filing.complete
  "One pass over a complete EDGAR submission.

  The string is the web .txt or a daily .nc member. file-structure reads
  it once and keeps the original text. load-file copies one document body
  from that structure.

  The web file wraps a SEC-HEADER (CENTRAL INDEX KEY:, FILED AS OF DATE:).
  The .nc file wraps a SUBMISSION (<CIK>, <FILING-DATE>). The documents
  inside are the same. :header is the first filer, from either wrapper."
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as str]))

(defn- value-end
  "Index of the first <, CR, or LF at or after `start`."
  [^String s start]
  (let [n (.length s)]
    (loop [k start]
      (if (>= k n)
        n
        (let [c (.charAt s k)]
          (if (or (= c \<) (= c \return) (= c \newline))
            k
            (recur (inc k))))))))

(defn- value-at
  [^String s start]
  (let [end (value-end s start)]
    (when (< start end)
      (not-empty (str/trim (subs s start end))))))

(defn- tag-value
  "First <tag> value. The search is the whole tag, so <TYPE> does not
  match <FORM-TYPE>. The value ends at <, CR, or LF, which covers a closed
  tag and a .nc line with no closer."
  [^String s tag]
  (let [open (str "<" tag ">")]
    (when-let [i (str/index-of s open)]
      (value-at s (+ i (.length open))))))

(defn- colon-value
  [^String s label]
  (when-let [i (str/index-of s label)]
    (value-at s (+ i (.length label)))))

(defn- iso-day
  [yyyymmdd]
  (when (and yyyymmdd (re-matches #"\d{8}" yyyymmdd))
    (str (subs yyyymmdd 0 4) "-" (subs yyyymmdd 4 6) "-" (subs yyyymmdd 6 8))))

(defn- parse-long-safe
  [s]
  (when (not-empty s)
    (try (Long/parseLong s)
         (catch Exception _ nil))))

(defn- header
  [^String preamble]
  {:cik (parse-long-safe (or (tag-value preamble "CIK")
                             (colon-value preamble "CENTRAL INDEX KEY:")))
   :name (or (tag-value preamble "CONFORMED-NAME")
             (colon-value preamble "COMPANY CONFORMED NAME:"))
   :filing-date (iso-day (or (tag-value preamble "FILING-DATE")
                             (colon-value preamble "FILED AS OF DATE:")))
   :form (or (tag-value preamble "TYPE")
             (colon-value preamble "CONFORMED SUBMISSION TYPE:"))
   :accession (or (tag-value preamble "ACCESSION-NUMBER")
                  (colon-value preamble "ACCESSION NUMBER:"))})

(defn- document
  [^String s i j]
  (let [text-open (str/index-of s "<TEXT>" i)
        in-doc? (and text-open (< text-open j))
        header-end (if in-doc? text-open j)
        head (subs s (+ i 10) header-end)
        text-close (when in-doc? (str/last-index-of s "</TEXT>" j))
        row {:seq (parse-long-safe (tag-value head "SEQUENCE"))
             :filename (tag-value head "FILENAME")
             :type (tag-value head "TYPE")}]
    (if (and text-close (< text-open text-close j))
      (assoc row :start (+ text-open 6) :end text-close)
      row)))

(defn- files
  [^String s from]
  (loop [from from acc []]
    (if-let [i (str/index-of s "<DOCUMENT>" from)]
      (if-let [j (str/index-of s "</DOCUMENT>" i)]
        (recur (+ j 11) (conj acc (document s i j)))
        acc)
      acc)))

(defn file-structure
  "One pass over a complete submission string.

  {:header {:cik :name :filing-date :form :accession}
   :files [{:seq :filename :type :start :end} ...]
   :text <the original string>}

  :start and :end index :text. Bodies are not copied here. :seq is a long.
  :type is the document <TYPE> line. The first filer wins."
  [complete-text]
  (let [s (or complete-text "")
        cut (or (str/index-of s "<DOCUMENT>") (.length s))]
    {:text s
     :header (header (subs s 0 cut))
     :files (files s cut)}))

(defn- extension?
  [filename extension]
  (and filename
       extension
       (str/ends-with? (str/lower-case filename)
                       (str/lower-case extension))))

(defn load-file
  "TEXT body of the first file whose name ends with `extension` and whose
  :type equals `type`. Nil when none match.

  The body is the exact text between <TEXT> and </TEXT>, including an
  <XML> wrapper. `extension` is compared case-insensitively, so \".xml\"
  matches primary_doc.XML and not an .html document of the same type."
  [structure extension type]
  (when-let [text (:text structure)]
    (some (fn [file]
            (when (and (:start file)
                       (:end file)
                       (= type (:type file))
                       (extension? (:filename file) extension))
              (subs text (:start file) (:end file))))
          (:files structure))))
