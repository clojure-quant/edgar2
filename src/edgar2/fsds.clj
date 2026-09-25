(ns edgar2.fsds
  "SEC Financial Statement Data Sets (FSDS) — bulk 10-K overview."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.core :as edgar]
            [edgar.fsds :as dera]
            [edgar2.tickers :as tickers])
  (:import [java.io BufferedReader InputStreamReader]
           [java.nio.charset StandardCharsets]
           [java.time LocalDate]
           [java.util.zip ZipFile]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def filer-info-path "data/filer-info.edn")
(def fsds-dir "data/fsds")
(def exchange-url "https://www.sec.gov/files/company_tickers_exchange.json")

(def revenue-tags
  ["RevenueFromContractWithCustomerExcludingAssessedTax"
   "Revenues"
   "SalesRevenueNet"
   "SalesRevenueGoodsNet"
   "RevenueFromContractWithCustomerIncludingAssessedTax"
   "RevenuesNetOfInterestExpense"
   "InterestAndDividendIncomeOperating"
   "PremiumsEarnedNet"
   "RealEstateRevenueNet"])

(def employee-tags ["EntityNumberOfEmployees"])
(def float-tags ["EntityPublicFloat"])
(def shares-tags ["EntityCommonStockSharesOutstanding"
                  "CommonStockSharesOutstanding"
                  "WeightedAverageNumberOfSharesOutstandingBasic"])

(def overview-tags
  (set (concat revenue-tags employee-tags float-tags shares-tags)))

(def afs-labels
  {"LAF" "Large accelerated filer"
   "1-LAF" "Large accelerated filer"
   "ACC" "Accelerated filer"
   "2-ACC" "Accelerated filer"
   "SRA" "Smaller reporting accelerated filer"
   "3-SRA" "Smaller reporting accelerated filer"
   "NON" "Non-accelerated filer"
   "4-NON" "Non-accelerated filer"
   "SML" "Smaller reporting company"
   "5-SML" "Smaller reporting company"})

(def sic-major-groups
  {"01" "Agricultural production — crops"
   "02" "Agricultural production — livestock"
   "07" "Agricultural services"
   "08" "Forestry"
   "09" "Fishing, hunting, and trapping"
   "10" "Metal mining"
   "12" "Coal mining"
   "13" "Oil and gas extraction"
   "14" "Nonmetallic minerals"
   "15" "Building construction"
   "16" "Heavy construction"
   "17" "Special trade contractors"
   "20" "Food and kindred products"
   "21" "Tobacco"
   "22" "Textile mill products"
   "23" "Apparel"
   "24" "Lumber and wood"
   "25" "Furniture"
   "26" "Paper"
   "27" "Printing and publishing"
   "28" "Chemicals"
   "29" "Petroleum refining"
   "30" "Rubber and plastics"
   "31" "Leather"
   "32" "Stone, clay, glass, concrete"
   "33" "Primary metals"
   "34" "Fabricated metals"
   "35" "Industrial machinery and computers"
   "36" "Electronic and electrical equipment"
   "37" "Transportation equipment"
   "38" "Instruments"
   "39" "Miscellaneous manufacturing"
   "40" "Railroad transportation"
   "41" "Local transit"
   "42" "Motor freight"
   "43" "US Postal Service"
   "44" "Water transportation"
   "45" "Air transportation"
   "46" "Pipelines (except natural gas)"
   "47" "Transportation services"
   "48" "Communications"
   "49" "Electric, gas, and sanitary services"
   "50" "Wholesale — durable"
   "51" "Wholesale — nondurable"
   "52" "Building materials and garden"
   "53" "General merchandise stores"
   "54" "Food stores"
   "55" "Auto dealers and gas stations"
   "56" "Apparel stores"
   "57" "Home furniture stores"
   "58" "Eating and drinking places"
   "59" "Miscellaneous retail"
   "60" "Depository institutions"
   "61" "Non-depository credit"
   "62" "Security and commodity brokers"
   "63" "Insurance carriers"
   "64" "Insurance agents and brokers"
   "65" "Real estate"
   "67" "Holding and investment offices"
   "70" "Hotels and lodging"
   "72" "Personal services"
   "73" "Business services"
   "75" "Automotive repair and services"
   "76" "Miscellaneous repair"
   "78" "Motion pictures"
   "79" "Amusement and recreation"
   "80" "Health services"
   "81" "Legal services"
   "82" "Educational services"
   "83" "Social services"
   "84" "Museums and gardens"
   "86" "Membership organizations"
   "87" "Engineering, accounting, research"
   "88" "Private households"
   "89" "Services, nec"
   "91" "Executive and general government"
   "92" "Justice and public order"
   "93" "Public finance"
   "94" "Human resource programs"
   "95" "Environmental and housing programs"
   "96" "Economic programs"
   "97" "National security"
   "99" "Nonclassifiable"})

(defn save-filer-info!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str filer-info-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file filer-info-path))))

(defn fmt-revenue
  [v]
  (if v
    (format "%.0fM" (/ (double v) 1.0e6))
    "-"))

(defn parse-num
  [s]
  (when (and s (not (str/blank? (str s))))
    (try (Double/parseDouble (str s))
         (catch Exception _ nil))))

(defn yyyymmdd->iso
  [s]
  (let [d (re-find #"\d{8}" (str s))]
    (when d
      (str (subs d 0 4) "-" (subs d 4 6) "-" (subs d 6 8)))))

(defn pad-fye
  [s]
  (when (and s (not (str/blank? (str s))))
    (format "%04d" (long (parse-num s)))))

(defn sic-code
  [s]
  (when-let [n (parse-num s)]
    (format "%04d" (long n))))

(defn sic-description
  [sic]
  (when sic
    (get sic-major-groups (subs (str sic) 0 2))))

(defn category-label
  [afs]
  (when (and afs (not (str/blank? (str afs))))
    (or (afs-labels (str/upper-case (str afs)))
        (afs-labels (str/upper-case (last (str/split (str afs) #"-"))))
        (str afs))))

(defn recent-quarters
  "Calendar quarters newest-first, starting at the current quarter."
  [n]
  (let [today (LocalDate/now)]
    (loop [y (.getYear today)
           q (inc (quot (dec (.getMonthValue today)) 3))
           acc []]
      (if (>= (count acc) n)
        acc
        (recur (if (= q 1) (dec y) y)
               (if (= q 1) 4 (dec q))
               (conj acc [y q]))))))

(defn download-fsds-quarter
  [year quarter]
  (let [path (str (io/file fsds-dir (str year "q" quarter ".zip")))]
    (if (.exists (io/file path))
      (do (println (format "  have %dq%d" year quarter))
          path)
      (try
        (println (format "  download %dq%d …" year quarter))
        (flush)
        (dera/download-quarter! year quarter fsds-dir)
        (catch Exception ex
          (println (format "  skip %dq%d (%s)" year quarter (.getMessage ex)))
          nil)))))

(defn reduce-zip-tsv
  [zip-path entry-name rf init]
  (with-open [zf (ZipFile. (str zip-path))]
    (let [entry (.getEntry zf entry-name)]
      (when-not entry
        (throw (ex-info (str entry-name " missing in " zip-path)
                        {:zip zip-path :entry entry-name})))
      (with-open [r (BufferedReader.
                     (InputStreamReader. (.getInputStream zf entry)
                                         StandardCharsets/UTF_8))]
        (let [headers (mapv keyword (str/split (str/trimr (.readLine r)) #"\t"))]
          (loop [acc init]
            (if-let [line (.readLine r)]
              (recur (rf acc (zipmap headers (str/split (str/trimr line) #"\t" -1))))
              acc)))))))

(defn newer-sub?
  [a b]
  (pos? (compare (str (:filed a)) (str (:filed b)))))

(defn collect-10k-subs
  "Latest 10-K submission per CIK from FSDS SUB tables."
  [zip-paths]
  (reduce
   (fn [by-cik zip-path]
     (println (format "  parse SUB %s" (.getName (io/file zip-path))))
     (flush)
     (reduce-zip-tsv
      zip-path "sub.txt"
      (fn [acc row]
        (if (or (not= "10-K" (str (:form row)))
                (str/blank? (str (:cik row))))
          acc
          (let [cik (tickers/pad-cik (:cik row))
                sub {:cik cik
                     :adsh (:adsh row)
                     :sic (sic-code (:sic row))
                     :afs (:afs row)
                     :fye (pad-fye (:fye row))
                     :period (str (:period row))
                     :filed (str (:filed row))}]
            (if (or (nil? (get acc cik)) (newer-sub? sub (get acc cik)))
              (assoc acc cik sub)
              acc))))
      by-cik))
   {}
   zip-paths))

(defn better-fact
  [prev row period]
  (let [ddate (str (:ddate row))
        qtrs (or (some-> (:qtrs row) parse-num long) -1)
        val (parse-num (:value row))
        cand {:val val :ddate ddate :qtrs qtrs}
        period (str period)]
    (cond
      (nil? val) prev
      (nil? prev) cand
      (and (= ddate period) (not= (:ddate prev) period)) cand
      (and (= ddate period) (= (:ddate prev) period) (> qtrs (:qtrs prev))) cand
      (and (not= (:ddate prev) period) (pos? (compare ddate (:ddate prev)))) cand
      :else prev)))

(defn collect-overview-facts
  "NUM facts for the latest-10-K accessions only."
  [zip-paths subs-by-cik]
  (let [period-by-adsh (into {} (map (juxt :adsh :period) (vals subs-by-cik)))
        wanted (set (keys period-by-adsh))]
    (reduce
     (fn [facts zip-path]
       (println (format "  parse NUM %s" (.getName (io/file zip-path))))
       (flush)
       (reduce-zip-tsv
        zip-path "num.txt"
        (fn [acc row]
          (let [adsh (:adsh row)
                tag (:tag row)]
            (if (or (not (wanted adsh))
                    (not (overview-tags tag))
                    (not (str/blank? (str (:coreg row))))
                    (not (str/blank? (str (:segments row)))))
              acc
              (update-in acc [adsh tag]
                         better-fact row (period-by-adsh adsh)))))
        facts))
     {}
     zip-paths)))

(defn fact-val
  [facts adsh tags qtrs]
  (some (fn [tag]
          (let [f (get-in facts [adsh tag])]
            (when (and f (or (nil? qtrs) (= qtrs (:qtrs f))))
              (:val f))))
        tags))

(defn fact-val*
  [facts adsh tags qtrs]
  (or (fact-val facts adsh tags qtrs)
      (fact-val facts adsh tags nil)))

(defn fetch-exchanges
  "ticker → exchange from company_tickers_exchange.json (one request)."
  []
  (let [payload (edgar/edgar-get exchange-url)
        fields (mapv keyword (:fields payload))]
    (into {}
          (for [row (:data payload)
                :let [m (zipmap fields row)]]
            [(str/upper-case (str (:ticker m))) (:exchange m)]))))

(defn build-filer-info-row
  [filer sub facts exchange]
  (let [adsh (:adsh sub)
        sic (:sic sub)]
    {:ticker (:ticker filer)
     :cik (:cik filer)
     :name (:name filer)
     :exchange exchange
     :sic sic
     :sic-description (sic-description sic)
     :category (category-label (:afs sub))
     :fiscal-year-end (:fye sub)
     :last-10k-date (yyyymmdd->iso (:filed sub))
     :last-10k-period (yyyymmdd->iso (:period sub))
     :last-10k-accession adsh
     :last-10k-revenue (fact-val* facts adsh revenue-tags 4)
     :employees (some-> (fact-val* facts adsh employee-tags 0) long)
     :public-float (fact-val* facts adsh float-tags 0)
     :shares-outstanding (some-> (fact-val* facts adsh shares-tags 0) long)}))

(defn filer-info
  "Build an overview of all primary filers from a few SEC bulk files
  (FSDS quarters + company_tickers_exchange.json), not one API call per CIK.

  Usage: clj -X:filer-info
         clj -X:filer-info :limit 10"
  ([] (filer-info {}))
  ([{:keys [limit]}]
   (e/init! identity-header)
   (e/enable-disk-cache! :dir "data/edgar-cache")
   (let [all (tickers/load-tickers)
         filers (vec (cond->> all limit (take (long limit))))
         total (count all)
         n (count filers)]
     (println (format "filer-info  overview from FSDS bulk files  %d filers  → %s"
                      n filer-info-path))
     (println "Downloading recent FSDS quarters (cached after the first run)…")
     (let [zips (loop [qs (recent-quarters 8) acc []]
                  (cond
                    (or (>= (count acc) 4) (empty? qs)) acc
                    :else
                    (let [path (apply download-fsds-quarter (first qs))]
                      (recur (rest qs) (cond-> acc path (conj path))))))]
       (when (empty? zips)
         (throw (ex-info "No FSDS quarter zips available" {:dir fsds-dir})))
       (println (format "Using %d quarter zip(s)" (count zips)))
       (let [subs (collect-10k-subs zips)
             facts (collect-overview-facts zips subs)
             exchanges (do (println "  fetch company_tickers_exchange.json")
                           (flush)
                           (fetch-exchanges))
             rows (mapv (fn [filer]
                          (build-filer-info-row
                           filer
                           (get subs (:cik filer))
                           facts
                           (get exchanges (:ticker filer))))
                        filers)]
         (doseq [[i row] (map-indexed vector rows)]
           (println (format "%d/%d  %s  %s  %s  10-K=%s  period=%s  rev=%s  sic=%s  %s  shares=%s  emp=%s"
                            (inc i) (or limit total)
                            (:ticker row) (:cik row) (:name row)
                            (or (:last-10k-date row) "-")
                            (or (:last-10k-period row) "-")
                            (fmt-revenue (:last-10k-revenue row))
                            (or (:sic row) "-")
                            (or (:exchange row) "-")
                            (or (:shares-outstanding row) "-")
                            (or (:employees row) "-")))
           (flush))
         (save-filer-info! rows)
         (when (.exists (io/file "data/filer-info.log.edn"))
           (io/delete-file "data/filer-info.log.edn" true))
         (println (format "Wrote %s  (%d filers, %d with a 10-K in the FSDS window)"
                          filer-info-path
                          (count rows)
                          (count (filter :last-10k-date rows))))
         rows)))))
