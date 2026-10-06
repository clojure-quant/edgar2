(ns edgar2.web.filing
  "Serve the latest annual report, cached under data/filings."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.filing :as ef]
            [edgar2.filing :as filing]))

(defn- content-type
  [file]
  (let [name (str/lower-case (.getName file))]
    (if (or (str/ends-with? name ".htm")
            (str/ends-with? name ".html"))
      "text/html; charset=utf-8"
      "text/plain; charset=utf-8")))

(defn annual-report-file
  "Path of the latest 10-K, 20-F, or 40-F primary document.
  Downloads it into data/filings when it is not already there."
  [ticker]
  (filing/ensure-identity!)
  (let [cik (filing/resolve-filer ticker)
        form (filing/annual-form cik)
        f (e/filing cik :form form)
        primary (when f (ef/primary-doc (ef/filing-index f)))]
    (when primary
      (let [path (io/file filing/filing-dir
                          (str (:form f))
                          (str (:cik f))
                          (str (:accessionNumber f))
                          (str (:name primary)))]
        (if (.isFile path)
          path
          (when-let [saved (ef/filing-save! f filing/filing-dir)]
            (io/file saved)))))))

(defn annual-report-page
  "Hyper handler. Returns the cached filing as a raw response."
  [req]
  (let [ticker (some-> (get-in req [:hyper/route :path-params :ticker])
                       str str/trim not-empty str/upper-case)]
    (try
      (if-let [file (and ticker (annual-report-file ticker))]
        {:status 200
         :headers {"Content-Type" (content-type file)}
         :body file}
        {:status 404
         :headers {"Content-Type" "text/plain; charset=utf-8"}
         :body (str "No annual report for " ticker)})
      (catch Throwable e
        {:status 502
         :headers {"Content-Type" "text/plain; charset=utf-8"}
         :body (or (ex-message e) (str e))}))))
