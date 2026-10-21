(ns edgar2.adr
  "ADR listings from 20-F/40-F cover-page Security12bTitle."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.fsds :as dl]
            [edgar2.download.fsds-universe :as fsds]
            [edgar2.util.ticker-info :as ticker-info])
  (:import [java.time LocalDate]))

(def adr-path "data/adr.edn")

(def foreign-annual-forms
  #{"20-F" "20-F/A" "40-F" "40-F/A"})

(def ^:private ones-words
  {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
   "six" 6 "seven" 7 "eight" 8 "nine" 9})

(def ^:private teens-words
  {"ten" 10 "eleven" 11 "twelve" 12 "thirteen" 13 "fourteen" 14
   "fifteen" 15 "sixteen" 16 "seventeen" 17 "eighteen" 18 "nineteen" 19})

(def ^:private tens-words
  {"twenty" 20 "thirty" 30 "forty" 40 "fifty" 50
   "sixty" 60 "seventy" 70 "eighty" 80 "ninety" 90})

(def ^:private word->n
  (merge
   ones-words
   teens-words
   tens-words
   {"hundred" 100 "half" 0.5
    "one-half" 0.5 "one-third" (/ 1.0 3) "one-fourth" 0.25
    "one-fifth" 0.2 "one-sixth" (/ 1.0 6) "one-eighth" 0.125
    "one-tenth" 0.1}
   (into {}
         (for [[t tv] tens-words
               [o ov] ones-words]
           [(str t "-" o) (+ tv ov)]))))

(defn- amendment?
  [form]
  (str/includes? (str form) "/A"))

(defn- better-sub?
  "True when candidate `a` should replace current `b`."
  [a b]
  (let [cmp (compare (str (:filed a)) (str (:filed b)))]
    (cond
      (pos? cmp) true
      (neg? cmp) false
      :else
      (and (not (amendment? (:form a)))
           (amendment? (:form b))))))

(defn collect-foreign-annual-subs
  "Latest 20-F/40-F per CIK across FSDS SUB tables. Returns {cik sub}."
  [zip-paths]
  (reduce
   (fn [by-cik zip-path]
     (println (format "  parse SUB %s" (.getName (io/file zip-path))))
     (flush)
     (fsds/reduce-zip-tsv
      zip-path "sub.txt"
      (fn [acc row]
        (let [form (str (:form row))]
          (if (or (not (foreign-annual-forms form))
                  (str/blank? (str (:cik row)))
                  (str/blank? (str (:adsh row))))
            acc
            (let [cik (fsds/pad-cik (:cik row))
                  sub {:cik cik
                       :name (str/trim (str (:name row)))
                       :adsh (:adsh row)
                       :form form
                       :filed (str (:filed row))}]
              (if (or (nil? (get acc cik))
                      (better-sub? sub (get acc cik)))
                (assoc acc cik sub)
                acc)))))
      by-cik))
   {}
   zip-paths))

(defn- primary-html
  "Primary filing document, cached under data/ix-html."
  [cik adsh]
  (let [f (io/file "data/ix-html" (str adsh ".htm"))]
    (or (when (.exists f) (slurp f))
        (when-let [html (e/html {:cik (ticker-info/pad-cik cik)
                                 :accessionNumber (str adsh)})]
          (.mkdirs (.getParentFile f))
          (spit f html)
          html))))

(defn- strip-tags
  [s]
  (-> (or s "")
      (str/replace #"<[^>]+>" " ")
      (str/replace #"&nbsp;|&#160;" " ")
      (str/replace #"&#\d+;|&[a-zA-Z]+;" " ")
      (str/replace #"\s+" " ")
      str/trim))

(defn security12b-titles
  "All dei:Security12bTitle string values from inline XBRL HTML."
  [html]
  (when html
    (let [m (re-matcher
             #"(?is)<ix:nonNumeric\b[^>]*\bname=\"dei:Security12bTitle\"[^>]*>(.*?)</ix:nonNumeric>"
             html)]
      (->> (loop [acc []]
             (if (.find m)
               (recur (conj acc (strip-tags (.group m 1))))
               acc))
           (remove str/blank?)
           distinct
           vec))))

(defn adr-title?
  "True for American/Global Depositary Share/Receipt titles (not bank preferred DSs)."
  [title]
  (boolean
   (re-find #"(?i)american\s+depositary|global\s+depositary|\bADSs?\b|\bADRs?\b"
            (str title))))

(defn- normalize-ratio
  [n]
  (when (number? n)
    (let [d (double n)]
      (if (< (Math/abs (- d (Math/rint d))) 1e-9)
        (long (Math/rint d))
        d))))

(def ^:private word-ratio-re
  (let [words (->> (keys word->n)
                   (sort-by (comp - count))
                   (map #(java.util.regex.Pattern/quote %))
                   (str/join "|"))]
    {:represent (re-pattern
                 (str "(?i)represent(?:s|ing)\\s+(?:a\\s+)?(" words ")\\b"))
     :each (re-pattern
            (str "(?i)each\\s+(?:of\\s+which\\s+)?represents?\\s+(?:a\\s+)?("
                 words ")\\b"))}))

(defn- parse-number-token
  "Digit or number-word (including thirty-five) → number, or nil."
  [s]
  (when s
    (let [s (str/lower-case (str/trim s))]
      (or (word->n s)
          (when (re-matches #"\d+(?:\.\d+)?" s)
            (Double/parseDouble s))))))

(defn- bare-ads-title?
  "True when the title is only ADS/GDS wording (no share count)."
  [title]
  (let [s (-> (str title)
              str/lower-case
              (str/replace #"[.,;:]" "")
              (str/replace #"\s+" " ")
              str/trim)]
    (boolean
     (re-matches
      #"(?:petrobras )?american depositary shares(?: \(ads?s?\))?(?: \(evidenced by american(?: depositary receipts)?\))?(?: as evidenced by american)?(?: \( each)?|global depositary shares"
      s))))

(defn parse-ratio
  "Ordinary shares per one ADR/ADS from a Security12bTitle, or nil.
  Bare \"American Depositary Shares\" (no count) defaults to 1."
  [title]
  (let [t (-> (str title)
              str/lower-case
              (str/replace #"\s+" " ")
              (str/replace #"shareswith" "shares with"))
        word (str "(?:" (->> (keys word->n)
                             (sort-by (comp - count))
                             (map #(java.util.regex.Pattern/quote %))
                             (str/join "|"))
                  ")")
        num (str "(?:\\d+(?:\\.\\d+)?|" word ")")]
    (normalize-ratio
     (or
      ;; representing thirty-five (35) … / representing (35) …
      (when-let [[_ n] (re-find #"represent(?:s|ing)\s+(?:[a-z-]+\s+)?\((\d+(?:\.\d+)?)\)" t)]
        (Double/parseDouble n))
      (when-let [[_ a b] (re-find #"represent(?:s|ing)\s+(\d+)\s*/\s*(\d+)" t)]
        (/ (Double/parseDouble a) (Double/parseDouble b)))
      ;; every 3 ADSs represent 40 shares → 40/3
      (when-let [[_ ads shares]
                 (re-find (re-pattern
                           (str "every\\s+(" num ")\\s+(?:ads|adss|adrs?|of which)\\s+represent\\s+("
                                num ")\\b"))
                          t)]
        (/ (double (parse-number-token shares))
           (double (parse-number-token ads))))
      ;; each two ADSs represent one → 1/2
      (when-let [[_ ads shares]
                 (re-find (re-pattern
                           (str "each\\s+(" num ")\\s+(?:ads|adss|adrs?)\\s+represent\\s+("
                                num ")\\b"))
                          t)]
        (/ (double (parse-number-token shares))
           (double (parse-number-token ads))))
      ;; right to receive N
      (when-let [[_ n]
                 (re-find (re-pattern
                           (str "right to receive\\s+(" num ")\\b"))
                          t)]
        (parse-number-token n))
      (when-let [[_ w] (re-find (:represent word-ratio-re) t)]
        (word->n w))
      (when-let [[_ n] (re-find #"represent(?:s|ing)\s+(?:a\s+)?(\d+(?:\.\d+)?)\b" t)]
        (Double/parseDouble n))
      (when-let [[_ a b] (re-find #"each\s+(?:of\s+which\s+)?represents?\s+(\d+)\s*/\s*(\d+)" t)]
        (/ (Double/parseDouble a) (Double/parseDouble b)))
      ;; each representing … (80) …
      (when-let [[_ n] (re-find #"each\s+(?:of\s+which\s+)?represents?\s+(?:[a-z-]+\s+)?\((\d+(?:\.\d+)?)\)" t)]
        (Double/parseDouble n))
      (when-let [[_ w] (re-find (:each word-ratio-re) t)]
        (word->n w))
      (when-let [[_ n] (re-find #"each\s+(?:of\s+which\s+)?represents?\s+(?:a\s+)?(\d+(?:\.\d+)?)\b" t)]
        (Double/parseDouble n))
      (when (bare-ads-title? title) 1)))))

(defn- filing-date
  [filed]
  (fsds/yyyymmdd->iso filed))

(defn- adr-row
  [sub titles]
  (when-let [title (some #(when (adr-title? %) %) titles)]
    (when-let [{:keys [ticker]} (ticker-info/primary-ticker-for-cik (:cik sub))]
      (let [ratio (parse-ratio title)]
        (cond-> {:ticker (str ticker)
                 :cik (:cik sub)
                 :filing-date (filing-date (:filed sub))
                 :title title
                 :form (:form sub)}
          ratio (assoc :ratio ratio))))))
(defn run
  "Scan latest 20-F/40-F cover pages for ADRs. Writes data/adr.edn.

  Usage: clj -X:adr
         clj -X:adr :limit 20"
  ([] (run {}))
  ([{:keys [limit]}]
   (e/init! dl/identity-header)
   (e/enable-disk-cache! :dir "data/edgar-cache")
   (println (format "adr  → %s" adr-path))
   (println "Ensuring recent FSDS quarters…")
   (let [zips (dl/ensure-quarters!)]
     (when (empty? zips)
       (throw (ex-info "No FSDS quarter zips available" {:dir dl/fsds-dir})))
     (println (format "Using %d quarter zip(s)" (count zips)))
     (let [by-cik (collect-foreign-annual-subs zips)
           subs (vec (cond->> (sort-by (juxt :name :cik) (vals by-cik))
                       limit (take (long limit))))
           n (count subs)
           _ (println (format "Scanning %d foreign annual filings for Security12bTitle…" n))
           rows (->> subs
                     (map-indexed
                      (fn [i sub]
                        (when (zero? (mod (inc i) 50))
                          (println (format "  %d/%d" (inc i) n))
                          (flush))
                        (try
                          (let [html (primary-html (:cik sub) (:adsh sub))
                                titles (security12b-titles html)]
                            (adr-row sub titles))
                          (catch Throwable ex
                            (println (format "  skip %s %s (%s)"
                                             (:cik sub) (:adsh sub)
                                             (.getMessage ex)))
                            nil))))
                     (remove nil?)
                     (sort-by :ticker)
                     vec)]
       (dataset/write-dataset! adr-path
                               {:download-date (str (LocalDate/now))}
                               rows)
       (println (format "Wrote %s  (%d ADRs; %d with :ratio)"
                        adr-path
                        (count rows)
                        (count (filter :ratio rows))))
       rows))))
