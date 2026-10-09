(ns edgar2.web.filing
  "Serve filing documents, cached under data/filings."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.core :as ec]
            [edgar.filing :as ef]
            [edgar2.filing :as filing]))

(defn- content-type
  [file]
  (let [name (str/lower-case (.getName file))]
    (cond
      (or (str/ends-with? name ".htm")
          (str/ends-with? name ".html")) "text/html; charset=utf-8"
      (or (str/ends-with? name ".xml")
          (str/ends-with? name ".xsd")) "application/xml; charset=utf-8"
      (str/ends-with? name ".pdf") "application/pdf"
      (str/ends-with? name ".txt") "text/plain; charset=utf-8"
      (or (str/ends-with? name ".jpg")
          (str/ends-with? name ".jpeg")) "image/jpeg"
      (str/ends-with? name ".gif") "image/gif"
      (str/ends-with? name ".png") "image/png"
      (str/ends-with? name ".svg") "image/svg+xml"
      (str/ends-with? name ".css") "text/css; charset=utf-8"
      :else "application/octet-stream")))

(defn- file-response
  [file]
  {:status 200
   :headers {"Content-Type" (content-type file)}
   :body file})

(defn- text-response
  [status body]
  {:status status
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body body})

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
        (file-response file)
        (text-response 404 (str "No annual report for " ticker)))
      (catch Throwable e
        (text-response 502 (or (ex-message e) (str e)))))))

(defn cached-primary
  "Path of a filing's primary document.
  Downloads it into data/filings when it is not already there."
  [f]
  (let [primary (:primaryDocument f)
        path (when (and primary (:form f) (:cik f) (:accessionNumber f))
               (io/file filing/filing-dir
                        (str (:form f))
                        (str (:cik f))
                        (str (:accessionNumber f))
                        (str primary)))]
    (cond
      (nil? path) nil
      (.isFile path) path
      :else (when-let [saved (ef/filing-save! f filing/filing-dir)]
              (io/file saved)))))

(defn- padded-cik
  [cik]
  (when-let [s (some-> cik str str/trim not-empty)]
    (when (re-matches #"\d+" s)
      (format "%010d" (Long/parseLong s)))))

(defn- same-cik?
  [a b]
  (and a b (= (parse-long (str a)) (parse-long (str b)))))

(defn- http-404?
  [e]
  (and (instance? clojure.lang.ExceptionInfo e)
       (= 404 (:status (ex-data e)))))

(defn- try-primary
  "Download `f`, or nil when SEC has no document at that CIK."
  [f]
  (try
    (cached-primary f)
    (catch clojure.lang.ExceptionInfo e
      (if (http-404? e) nil (throw e)))))

(def ^:private binary-extensions
  #{".pdf" ".gif" ".jpg" ".jpeg" ".png" ".zip" ".xls" ".xlsx" ".doc" ".docx"})

(defn- binary-filename?
  [name]
  (let [lower (str/lower-case (str name))]
    (boolean (some #(str/ends-with? lower %) binary-extensions))))

(defn- safe-doc-name
  "A single filing filename. Rejects path segments such as \"..\"."
  [doc]
  (when-let [s (some-> doc str str/trim not-empty)]
    (when (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" s)
      s)))

(defn- cache-file
  [f doc-name]
  (when (and doc-name (:form f) (:cik f) (:accessionNumber f))
    (io/file filing/filing-dir
             (str (:form f))
             (str (:cik f))
             (str (:accessionNumber f))
             doc-name)))

(defn- download-doc!
  [f doc-name path]
  (.mkdirs (.getParentFile path))
  (let [url (ef/filing-doc-url f doc-name)]
    (if (binary-filename? doc-name)
      (let [bytes (ec/edgar-get-bytes url)]
        (with-open [out (java.io.FileOutputStream. path)]
          (.write out ^bytes bytes)))
      (spit path (ec/edgar-get url :raw? true))))
  path)

(defn- try-doc
  "Cached path of one named document, or nil when SEC has none at this CIK."
  [f doc-name]
  (when-let [path (cache-file f doc-name)]
    (cond
      (.isFile path) path
      :else (try
              (download-doc! f doc-name path)
              (catch clojure.lang.ExceptionInfo e
                (if (http-404? e) nil (throw e)))))))

(defn raw-filing-file
  "Path of the primary document for an accession number.
  Downloads it into data/filings when it is not already there.

  `cik` is the issuer. The accession prefix is the submitter, which for
  agent-filed registration statements is a filing agent. Those documents
  are stored under the issuer CIK, so that path is tried first."
  [cik accession]
  (filing/ensure-identity!)
  (let [f (e/filing-by-accession accession)
        issuer (padded-cik cik)
        issuer-filing (if issuer (assoc f :cik issuer) f)]
    (or (try-primary issuer-filing)
        (when-not (same-cik? (:cik issuer-filing) (:cik f))
          (try-primary f)))))

(defn filing-doc-file
  "Path of one document in a filing, such as an image beside the HTML.
  `cik` is the issuer. The accession prefix is the submitter, so the issuer
  path is tried first, same as `raw-filing-file`."
  [cik accession doc-name]
  (filing/ensure-identity!)
  (let [doc (safe-doc-name doc-name)
        f (when doc (e/filing-by-accession accession))
        issuer (padded-cik cik)
        issuer-filing (if issuer (assoc f :cik issuer) f)]
    (when f
      (or (try-doc issuer-filing doc)
          (when-not (same-cik? (:cik issuer-filing) (:cik f))
            (try-doc f doc))))))

(defn- redirect
  [url]
  {:status 302
   :headers {"Location" url}
   :body ""})

(defn raw-filing-page
  "Hyper handler. Returns one filing document as a raw response.
  A request with no filename redirects to the primary document so relative
  image links in the HTML resolve next to it."
  [req]
  (let [cik (get-in req [:hyper/route :path-params :cik])
        accession (some-> (get-in req [:hyper/route :path-params :accession])
                          str str/trim not-empty)
        doc (safe-doc-name (get-in req [:hyper/route :path-params :doc]))]
    (try
      (cond
        (and accession doc)
        (if-let [file (filing-doc-file cik accession doc)]
          (file-response file)
          (text-response 404 (str "No document " doc " in " accession)))

        accession
        (if-let [file (raw-filing-file cik accession)]
          (redirect (str "/filing/" (or (padded-cik cik) cik)
                         "/" accession "/" (.getName file)))
          (text-response 404 (str "No filing for " accession)))

        :else
        (text-response 404 "No filing"))
      (catch clojure.lang.ExceptionInfo e
        (if (= :edgar.filing/not-found (:type (ex-data e)))
          (text-response 404 (or (ex-message e) "Filing not found"))
          (text-response 502 (or (ex-message e) (str e)))))
      (catch Throwable e
        (text-response 502 (or (ex-message e) (str e)))))))
