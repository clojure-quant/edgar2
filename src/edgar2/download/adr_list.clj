(ns edgar2.download.adr-list
  "Fetch ADR lists from Citi, BNY, and JPM → data/adr-list.edn.

  One row per :ticker. :source is a set of providers that listed it
  (e.g. #{:jpm :bny}).

  Ratio is ordinary shares per one DR (Citi ORD:DR as a/b; BNY/JPM DR:ORD as b/a).

  Usage: clj -X:adr-list"
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [jsonista.core :as json])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset Charset StandardCharsets]
           [java.time Duration LocalDate]
           [java.util.concurrent Executors TimeUnit]))

(def listed-path "data/adr-list.edn")
(def otc-path "data/adr-list-otc.edn")

(def user-agent "Mozilla/5.0 (compatible; edgar2-adr-list/1.0)")

;; Prefer these when picking company-name / resolving ratio ties.
;; BNY first: empirically freshest on ADS ratio changes (JPM often lags).
(def source-pref [:bny :jpm :citi])

(def ^:private colon-ratio-re
  #"^\s*(\d+(?:\.\d+)?)\s*:\s*(\d+(?:\.\d+)?)\s*$")

(def ^:private ticker-re #"^[A-Z0-9./-]{1,15}$")

;; BNY uses these when a program has no public exchange ticker (144A / Reg S / etc).
;; Collapsing hundreds of issuers onto "--" or "NA" produces bogus :conflict vectors.
(def ^:private placeholder-tickers
  #{"--" "NA" "N/A" "N.A." "NONE" "NULL"})

(defn- usable-ticker?
  [ticker]
  (let [t (some-> ticker str str/trim not-empty)]
    (boolean
     (and t
          (not (placeholder-tickers (str/upper-case t)))
          (re-matches ticker-re t)))))

(defn- http-client
  []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.connectTimeout (Duration/ofSeconds 30))
      (.build)))

(defn- http-get
  "GET `url` → body string. Optional `:charset` (default UTF-8)."
  ([client url] (http-get client url {}))
  ([client url {:keys [charset timeout-sec]
                :or {charset StandardCharsets/UTF_8
                     timeout-sec 120}}]
   (let [req (-> (HttpRequest/newBuilder)
                 (.uri (URI/create url))
                 (.timeout (Duration/ofSeconds (long timeout-sec)))
                 (.header "User-Agent" user-agent)
                 (.GET)
                 .build)
         resp (.send client req (HttpResponse$BodyHandlers/ofByteArray))
         status (.statusCode resp)]
     (when-not (= 200 status)
       (throw (ex-info (str "HTTP " status) {:url url :status status})))
     (String. ^bytes (.body resp) ^java.nio.charset.Charset charset))))

(defn- http-json
  [client url]
  (json/read-value (http-get client url) json/keyword-keys-object-mapper))

(defn- parse-colon-ratio
  [s]
  (when-let [[_ a b] (re-matches colon-ratio-re (str s))]
    [(Double/parseDouble a) (Double/parseDouble b)]))

(defn- ord-per-dr-from-ord-dr
  "Citi ORD:DR → ordinary shares per one DR."
  [s]
  (when-let [[a b] (parse-colon-ratio s)]
    (when-not (zero? b)
      (/ a b))))

(defn- ord-per-dr-from-dr-ord
  "BNY/JPM DR:ORD → ordinary shares per one DR."
  [s]
  (when-let [[a b] (parse-colon-ratio s)]
    (when-not (zero? a)
      (/ b a))))

(defn- normalize-ratio
  [n]
  (when (number? n)
    (let [d (double n)]
      (if (< (Math/abs (- d (Math/rint d))) 1e-9)
        (long (Math/rint d))
        d))))

(defn- ratio-key
  "Round for majority-vote comparison (matches Python round(x, 10))."
  [x]
  (when (number? x)
    (-> (double x) (* 1.0e10) Math/round (/ 1.0e10) double)))

(defn- strip-html
  [s]
  (-> (or s "")
      (str/replace #"<[^>]+>" " ")
      (str/replace #"&nbsp;|&#160;" " ")
      (str/replace #"&amp;" "&")
      (str/replace #"&quot;" "\"")
      (str/replace #"&lt;" "<")
      (str/replace #"&gt;" ">")
      (str/replace #"&#(\d+);"
                   (fn [[_ n]] (str (char (Integer/parseInt n)))))
      (str/replace #"\s+" " ")
      str/trim))

(defn- otc-ticker?
  "OTCBB / OTC ADR tickers conventionally end in Y."
  [ticker]
  (boolean (and (seq ticker) (str/ends-with? (str/upper-case ticker) "Y"))))

(defn fetch-citi
  [client]
  (let [url (str "https://depositaryreceipts.citi.com/adr/guides/"
                 "unspresource.aspx?pageId=3&subpageId=173")]
    (println "citi  ←" url)
    (flush)
    (let [html (http-get client url {:charset (Charset/forName "ISO-8859-1")})
          rows
          (into []
                (keep
                 (fn [tr]
                   (let [tds (->> (re-seq #"(?is)<td[^>]*>(.*?)</td>" tr)
                                  (mapv (comp strip-html second)))
                         ratio-i (->> tds
                                      (map-indexed vector)
                                      (some (fn [[i c]]
                                              (when (parse-colon-ratio c) i))))]
                     (when (and ratio-i (>= ratio-i 3) (>= (count tds) 7))
                       (let [ratio-s (nth tds ratio-i)
                             ticker (nth tds (dec ratio-i))
                             company (nth tds (- ratio-i 3))
                             ratio (ord-per-dr-from-ord-dr ratio-s)]
                         (when (and (usable-ticker? ticker)
                                    (not (#{"company" "issuer"} (str/lower-case company)))
                                    (not (re-matches #"[0-9A-Z]{9}" company))
                                    (some? ratio))
                           {:ticker ticker
                            :company-name company
                            :ratio (normalize-ratio ratio)
                            :source :citi}))))))
                (re-seq #"(?is)<tr[\s>].*?</tr>" html))]
      (println (format "citi  → %d rows" (count rows)))
      (flush)
      rows)))

(defn- bny-program-rank
  "Higher is better when several BNY rows share one ticker (different issuers).
  Prefer exchange-listed Level I/II/III over Reg S / 144A."
  [{:keys [dr-type exchange]}]
  (let [type-s (str/lower-case (str dr-type))
        ex-s (str/lower-case (str exchange))
        type-score (cond
                     (str/includes? type-s "level iii") 300
                     (str/includes? type-s "level ii") 200
                     (str/includes? type-s "level i") 100
                     (str/includes? type-s "144a") 10
                     (str/includes? type-s "reg s") 20
                     :else 50)
        ex-score (cond
                   (str/includes? ex-s "nasdaq") 30
                   (str/includes? ex-s "nyse") 30
                   (str/includes? ex-s "otc") 15
                   (or (str/blank? ex-s) (= ex-s "--")) 0
                   :else 10)]
    (+ type-score ex-score)))

(defn- dedupe-bny-by-ticker
  "One BNY row per ticker. Placeholder tickers already dropped; remaining
  collisions are distinct issuers that reused a symbol (e.g. TOUR)."
  [rows]
  (->> rows
       (group-by :ticker)
       (map (fn [[_ticker xs]]
              (if (= 1 (count xs))
                (first xs)
                (let [best (apply max-key bny-program-rank xs)]
                  (println (format "bny   ticker collision %s → keep %s (dropped %s)"
                                   (:ticker best)
                                   (:company-name best)
                                   (->> xs
                                        (remove #(= % best))
                                        (map :company-name)
                                        (str/join "; "))))
                  best))))
       (sort-by :ticker)
       vec))

(defn fetch-bny
  [client]
  (let [url (str "https://www.adrbny.com/bin/adr/export/drDirectoryList"
                 "?count=5000&start=0&drPage=directory&searchType=1")]
    (println "bny   ← directory API")
    (flush)
    (let [raw (http-get client url)
          parsed (json/read-value raw json/keyword-keys-object-mapper)
          ;; API sometimes double-encodes JSON as a string.
          d (if (string? parsed)
              (json/read-value parsed json/keyword-keys-object-mapper)
              parsed)
          items (get-in d [:data :drdirectory])
          skipped (atom 0)
          rows (into []
                     (keep
                      (fn [it]
                        (let [ticker (str/trim (str (or (:drTicker it) "")))
                              ratio (some-> (:drRatio it) ord-per-dr-from-dr-ord normalize-ratio)
                              name (-> (str (or (:drTx it) ""))
                                       (str/replace #"^\^+" "")
                                       str/trim)]
                          (cond
                            (not (usable-ticker? ticker))
                            (do (swap! skipped inc) nil)

                            (nil? ratio)
                            nil

                            :else
                            {:ticker ticker
                             :company-name name
                             :ratio ratio
                             :source :bny
                             :dr-type (str (or (:drType it) ""))
                             :exchange (str (or (:drExchange it) ""))}))))
                     items)
          deduped (dedupe-bny-by-ticker rows)]
      (println (format "bny   → %d rows (API count=%s, skipped-placeholder=%d, collisions-deduped=%d)"
                       (count deduped) (:count d) @skipped
                       (- (count rows) (count deduped))))
      (flush)
      ;; Strip ranking helpers before merge.
      (mapv #(dissoc % :dr-type :exchange) deduped))))

(defn- jpm-program-ratio
  [client cusip]
  (try
    (let [xid (get-in (http-json client
                                 (str "https://adr.api.wsod.com/jpmadr-public/v1/dr/xid/"
                                      cusip))
                      [:data :xid])
          det (:data (http-json client
                                (str "https://adr.api.wsod.com/jpmadr-public/v1/dr/programDetails/"
                                     xid)))
          ratio (some-> (:ratio det) ord-per-dr-from-dr-ord normalize-ratio)]
      {:ticker (str/trim (str (or (:ticker det) "")))
       :company-name (str/trim (str (or (:issuer det) "")))
       :ratio ratio
       :source :jpm
       :cusip cusip})
    (catch Throwable ex
      {:cusip cusip :error (.getMessage ex)})))

(defn fetch-jpm
  "Universe listing + per-CUSIP programDetails (parallel)."
  ([client] (fetch-jpm client 24))
  ([client workers]
   (let [url (str "https://adr.api.wsod.com/jpmadr-public/v1/drUniverse"
                  "?offset=0&limit=5000&sortBy=name&sortOrder=asc")]
     (println "jpm   ← universe + programDetails")
     (flush)
     (let [items (get-in (http-json client url) [:data :items])
           by-cusip (into {}
                          (keep (fn [i]
                                  (when-let [c (:cusip i)]
                                    [c i])))
                          items)
           cusips (vec (keys by-cusip))
           n (count cusips)
           _ (do (println (format "jpm   fetching ratios for %d CUSIPs (%d workers)…"
                                  n workers))
                 (flush))
           pool (Executors/newFixedThreadPool (int workers))
           t0 (System/currentTimeMillis)
           done (atom 0)
           errors (atom 0)
           futs (mapv (fn [cusip]
                        (.submit pool
                                 ^Callable
                                 (fn []
                                   (let [r (jpm-program-ratio client cusip)
                                         i (swap! done inc)]
                                     (when (or (zero? (mod i 200)) (= i n))
                                       (println (format "jpm   %d/%d (%.0fs, errors=%d)"
                                                        i n
                                                        (/ (- (System/currentTimeMillis) t0) 1000.0)
                                                        @errors))
                                       (flush))
                                     (when (or (:error r)
                                               (nil? (:ratio r))
                                               (str/blank? (:ticker r)))
                                       (swap! errors inc))
                                     r))))
                      cusips)]
       (try
         (let [rows (into []
                          (keep
                           (fn [fut]
                             (let [r (.get ^java.util.concurrent.Future fut)]
                               (when (and (not (:error r))
                                          (some? (:ratio r))
                                          (usable-ticker? (:ticker r)))
                                 (let [u (get by-cusip (:cusip r) {})
                                       name (or (not-empty (:company-name r))
                                                (str/trim (str (or (:name u) ""))))]
                                   {:ticker (:ticker r)
                                    :company-name name
                                    :ratio (:ratio r)
                                    :source :jpm})))))
                          futs)]
           (println (format "jpm   → %d rows (errors/skipped=%d)"
                            (count rows) @errors))
           (flush)
           rows)
         (finally
           (.shutdown pool)
           (.awaitTermination pool 60 TimeUnit/SECONDS)))))))

(defn- prefer-name
  [xs]
  (or (some (fn [src]
              (some #(when (and (= src (:source %))
                                (seq (:company-name %)))
                       (:company-name %))
                    xs))
            source-pref)
      (->> xs (map :company-name) (remove str/blank?) (sort-by (comp - count)) first)
      ""))

(defn- conflict-map
  "Per-provider ratios for a conflict row, e.g. {:jpm 5 :bny 1}."
  [xs]
  (into (array-map)
        (keep (fn [src]
                (when-let [rs (->> xs
                                   (filter #(= src (:source %)))
                                   (map :ratio)
                                   distinct
                                   vec
                                   not-empty)]
                  [src (if (= 1 (count rs)) (first rs) rs)]))
              source-pref)))

(defn- resolve-ratio
  "Majority vote on rounded ratio; ties broken by source-pref."
  [xs]
  (let [votes (frequencies (map (comp ratio-key :ratio) xs))
        best-count (apply max (vals votes))
        candidates (into #{} (keep (fn [[k v]] (when (= v best-count) k)) votes))
        conflict? (> (count votes) 1)
        picked (or (some (fn [src]
                           (some #(when (and (= src (:source %))
                                             (contains? candidates (ratio-key (:ratio %))))
                                    (:ratio %))
                                 xs))
                         source-pref)
                   (:ratio (first xs)))]
    {:ratio picked
     :conflict (when conflict? (conflict-map xs))}))

(defn merge-by-ticker
  "Collapse to one row per ticker; :source becomes a set of providers.
  Conflict rows also get :conflict {:jpm … :bny … :citi …}."
  [provider-rows]
  (let [by-ticker (group-by :ticker provider-rows)
        merged
        (->> by-ticker
             (map (fn [[ticker xs]]
                    (let [sources (into #{} (map :source) xs)
                          company (prefer-name xs)
                          {:keys [ratio conflict]} (resolve-ratio xs)]
                      (cond-> (array-map
                               :ticker ticker
                               :company-name company
                               :ratio ratio
                               :source sources)
                        conflict (assoc :conflict conflict)))))
             (sort-by :ticker)
             vec)
        conflicts (filterv :conflict merged)]
    (println (format "merge → %d unique tickers (ratio conflicts=%d)"
                     (count merged) (count conflicts)))
    (when (seq conflicts)
      (println)
      (println "ALERT: provider ratio differences (ordinary-shares-per-DR):")
      (doseq [{:keys [ticker company-name conflict ratio]} (sort-by :ticker conflicts)]
        (println (format "  %-10s  picked=%s  %s  %s"
                         ticker
                         ratio
                         (pr-str conflict)
                         (when (seq company-name)
                           (str "(" company-name ")")))))
      (println)
      (flush))
    {:rows merged :conflicts conflicts}))

(defn- write-edn!
  [path rows market]
  (dataset/write-dataset!
   path
   {:download-date (str (LocalDate/now))
    :market market
    :sources [:citi :bny :jpm]
    :ratio-unit "ordinary-shares-per-DR"}
   rows)
  (let [src-counts (frequencies (mapcat :source rows))
        multi (count (filter #(> (count (:source %)) 1) rows))
        n-conflict (count (filter :conflict rows))]
    (println (format "wrote %s  unique-tickers=%d  multi-source=%d  conflicts=%d  provider-hits=%s"
                     path (count rows) multi n-conflict (pr-str src-counts)))
    src-counts))

(defn split-and-write!
  [rows]
  (let [otc (filterv #(otc-ticker? (:ticker %)) rows)
        listed (filterv #(not (otc-ticker? (:ticker %))) rows)]
    (write-edn! listed-path listed "exchange-listed")
    (write-edn! otc-path otc "otc")
    (println (format "totals  exchange-listed=%d  otc=%d  all=%d"
                     (count listed) (count otc) (count rows)))
    {:listed listed :otc otc}))

(defn run
  "Fetch Citi + BNY + JPM directories and rebuild adr-list edn files.

  Usage: clj -X:adr-list
         clj -X:adr-list :workers 16"
  ([] (run {}))
  ([{:keys [workers] :or {workers 24}}]
   (let [client (http-client)
         citi (fetch-citi client)
         bny (fetch-bny client)
         jpm (fetch-jpm client (long workers))
         {:keys [rows conflicts]} (merge-by-ticker (concat citi bny jpm))
         out (split-and-write! rows)]
     (assoc out :conflicts conflicts))))
