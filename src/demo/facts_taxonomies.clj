(ns demo.facts-taxonomies
  "Filer counts by XBRL taxonomy in data/companyfacts.zip.

  Each CIK json is one filer. Taxonomy names are the keys of its top-level
  facts object. A closed-end fund is a filer that reports the cef taxonomy.
  ADRs have no taxonomy of their own; they report us-gaap or ifrs-full.

  Usage: clj -M -m demo.facts-taxonomies"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.download.facts :as dl])
  (:import [java.io BufferedInputStream]
           [java.nio.charset StandardCharsets]
           [java.util ArrayList]
           [java.util.zip ZipEntry ZipFile ZipInputStream]))

(set! *unchecked-math* true)

(defmacro ^:private move
  [i depth arr in-str esc expect-key key-start in-facts facts-depth pending]
  `(recur ~i ~depth ~arr ~in-str ~esc ~expect-key ~key-start ~in-facts ~facts-depth ~pending))

(defn- facts-key?
  "True when buf[start,end) is the ASCII key facts."
  [^bytes buf ^long start ^long end]
  (and (== (- end start) 5)
       (== (aget buf start) (byte 102))
       (== (aget buf (+ start 1)) (byte 97))
       (== (aget buf (+ start 2)) (byte 99))
       (== (aget buf (+ start 3)) (byte 116))
       (== (aget buf (+ start 4)) (byte 115))))

(defn- taxonomies
  "Keys of the top-level facts object. Concept names nested under a
  taxonomy are skipped."
  [^bytes buf]
  (let [^ArrayList acc (ArrayList.)
        n (alength buf)]
    (loop [i (long 0)
           depth (long 0)
           arr (long 0)
           in-str false
           esc false
           expect-key false
           key-start (long -1)
           in-facts false
           facts-depth (long -1)
           pending-facts false]
      (if (== i n)
        (vec acc)
        (let [b (long (aget buf i))
              i1 (unchecked-inc i)]
          ;; 34 "   92 \   123 {   125 }   91 [   93 ]   44 ,   58 :
          (cond
            in-str
            (cond
              esc
              (move i1 depth arr true false expect-key key-start in-facts facts-depth pending-facts)

              (== b 92)
              (move i1 depth arr true true expect-key key-start in-facts facts-depth pending-facts)

              (== b 34)
              (let [pending (or pending-facts
                                (and (>= key-start 0)
                                     (== depth 1)
                                     (facts-key? buf key-start i)))]
                (when (and (>= key-start 0)
                           in-facts
                           (== depth facts-depth))
                  (.add acc (String. buf (int key-start) (int (- i key-start))
                                     StandardCharsets/ISO_8859_1)))
                (move i1 depth arr false false false -1 in-facts facts-depth pending))

              :else
              (move i1 depth arr true false expect-key key-start in-facts facts-depth pending-facts))

            (== b 123)
            (let [depth1 (unchecked-inc depth)
                  enter (and pending-facts (not in-facts))]
              (move i1 depth1 arr false false true -1
                    (or in-facts enter)
                    (if enter depth1 facts-depth)
                    false))

            (== b 125)
            (let [leaving (and in-facts (== depth facts-depth))]
              (move i1 (unchecked-dec depth) arr false false false -1
                    (and in-facts (not leaving))
                    facts-depth
                    pending-facts))

            (== b 91)
            (move i1 depth (unchecked-inc arr) false false false -1 in-facts facts-depth pending-facts)

            (== b 93)
            (move i1 depth (unchecked-dec arr) false false false -1 in-facts facts-depth pending-facts)

            (== b 44)
            (move i1 depth arr false false (and (zero? arr) (pos? depth)) -1
                  in-facts facts-depth pending-facts)

            (== b 58)
            (move i1 depth arr false false false -1 in-facts facts-depth pending-facts)

            (== b 34)
            (let [capture (and expect-key
                               (or (== depth 1)
                                   (and in-facts (== depth facts-depth))))]
              (move i1 depth arr true false expect-key
                    (if capture i1 -1)
                    in-facts facts-depth pending-facts))

            :else
            (move i1 depth arr false false expect-key key-start in-facts facts-depth pending-facts)))))))

(defn- read-fully!
  [^java.io.InputStream in ^bytes buf]
  (let [n (alength buf)]
    (loop [off (long 0)]
      (when (< off n)
          (let [r (.read in buf off (- n off))]
          (cond
            (neg? r) (throw (ex-info "truncated companyfacts entry" {:off off :size n}))
            (zero? r) (throw (ex-info "companyfacts read stalled" {:off off :size n}))
            :else (recur (+ off r))))))))

(defn- file-count
  [zip-path]
  (with-open [zf (ZipFile. (str zip-path))]
    (.size zf)))

(defn run
  "Scan companyfacts.zip and print :taxonomy, :number-of-filers.

  A filer is one CIK file. A filer that reports several taxonomies is
  counted once under each."
  []
  (let [zip-path (dl/ensure-zip! {})
        total (file-count zip-path)
        t0 (System/nanoTime)]
    (println (format "facts-taxonomies  %d files  ← %s" total zip-path))
    (flush)
    (with-open [raw (io/input-stream zip-path)
                bin (BufferedInputStream. raw 1048576)
                zis (ZipInputStream. bin)]
      (loop [i (long 0)
             with-facts (long 0)
             counts (transient {})]
        (if-let [^ZipEntry entry (.getNextEntry zis)]
          (let [n (.getSize entry)
                json? (and (not (.isDirectory entry))
                           (.endsWith (.getName entry) ".json"))
                _ (when (and json? (neg? n))
                    (throw (ex-info "companyfacts entry has no size"
                                    {:name (.getName entry)})))
                taxes (when (and json? (pos? n))
                        (when (> n Integer/MAX_VALUE)
                          (throw (ex-info "companyfacts entry is too large"
                                          {:name (.getName entry) :size n})))
                        (let [buf (byte-array (int n))]
                          (read-fully! zis buf)
                          (taxonomies buf)))
                i1 (if json? (unchecked-inc i) i)]
            (.closeEntry zis)
            (when (and json? (or (zero? (mod i1 2000)) (== i1 total)))
              (println (format "  %d/%d" i1 total))
              (flush))
            (recur i1
                   (if (seq taxes) (unchecked-inc with-facts) with-facts)
                   (reduce (fn [m t]
                             (assoc! m t (inc (long (get m t 0)))))
                           counts
                           (when (seq taxes)
                             (set (map keyword taxes))))))
          (let [rows (->> (persistent! counts)
                          (map (fn [[taxonomy n]]
                                 {:taxonomy taxonomy
                                  :number-of-filers n}))
                          (sort-by (fn [{:keys [taxonomy number-of-filers]}]
                                     [(- (long number-of-filers)) (name taxonomy)]))
                          vec)
                secs (/ (- (System/nanoTime) t0) 1e9)]
            (println)
            (pprint/print-table [:taxonomy :number-of-filers] rows)
            (println (format "%d files, %d report a taxonomy  (%.0f s)"
                             total with-facts (double secs)))
            rows))))))

(defn -main
  [& _]
  (run)
  (shutdown-agents))
