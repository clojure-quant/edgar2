(ns edgar2.web.filing
  "Serve filing documents, cached under data/filings."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.core :as ec]
            [edgar.filing :as ef]
            [edgar2.util :as util]
            [edgar2.util.filing :as ufiling]))

(defn- content-type
  [file]
  (let [name (str/lower-case (.getName file))]
    (cond
      (or (str/ends-with? name ".htm")
          (str/ends-with? name ".html")) "text/html; charset=utf-8"
      (or (str/ends-with? name ".xml")
          (str/ends-with? name ".xsd")) "application/xml; charset=utf-8"
      (str/ends-with? name ".pdf") "application/pdf"
      (or (str/ends-with? name ".txt")
          (str/ends-with? name ".paper")) "text/plain; charset=utf-8"
      (or (str/ends-with? name ".jpg")
          (str/ends-with? name ".jpeg")) "image/jpeg"
      (str/ends-with? name ".gif") "image/gif"
      (str/ends-with? name ".png") "image/png"
      (str/ends-with? name ".svg") "image/svg+xml"
      (str/ends-with? name ".css") "text/css; charset=utf-8"
      :else "application/octet-stream")))

(defn- html-file?
  [file]
  (let [name (str/lower-case (.getName file))]
    (or (str/ends-with? name ".htm")
        (str/ends-with? name ".html"))))

(defn- pdf-file?
  [file]
  (str/ends-with? (str/lower-case (.getName file)) ".pdf"))

(defn- paper-filename?
  [name]
  (str/ends-with? (str/lower-case (str name)) ".paper"))

(def ^:private ix-ns
  "http://www.xbrl.org/2013/inlineXBRL")

(def ^:private layout-snippet
  "Cap HTML filings at 1200px and center them in the viewport."
  (str
   "<style id=\"edgar2-layout\">"
   "html,body{max-width:1200px;margin-left:auto;margin-right:auto}"
   "</style>"))

(def ^:private ix-highlight-snippet
  "Injected into every HTML filing. Marks ix facts that have both name and id."
  (str
   "<style id=\"edgar2-ix-hl\">"
   ".edgar2-ix{outline:1px solid red;cursor:help}"
   ".edgar2-ix-tip{position:fixed;z-index:2147483647;background:#111;color:#fff;"
   "padding:4px 8px;font:12px/1.3 sans-serif;pointer-events:none;"
   "max-width:48em;display:none;box-shadow:0 1px 4px rgba(0,0,0,.35);"
   "white-space:pre}"
   "</style>"
   "<script>(function(){"
   "var IXNS=" (pr-str ix-ns) ";"
   "function isIx(el){"
   "var t=(el.tagName||'').toLowerCase();"
   "return t.indexOf('ix:')===0||el.namespaceURI===IXNS;}"
   "function mark(){"
   "var els=document.getElementsByTagName('*');"
   "for(var i=0;i<els.length;i++){"
   "var el=els[i];"
   "if(!isIx(el))continue;"
   "if(!el.getAttribute('name')||!el.getAttribute('id'))continue;"
   "el.classList.add('edgar2-ix');}"
   "var tip=document.createElement('div');"
   "tip.className='edgar2-ix-tip';"
   "(document.body||document.documentElement).appendChild(tip);"
   "document.addEventListener('mouseover',function(e){"
   "var el=e.target;"
   "if(!el||!el.classList||!el.classList.contains('edgar2-ix'))return;"
   "var name=el.getAttribute('name')||'';"
   "var ctx=el.getAttribute('contextRef')||'';"
   "tip.textContent=ctx?name+'\\n'+ctx:name;"
   "tip.style.display='block';});"
   "document.addEventListener('mousemove',function(e){"
   "if(tip.style.display!=='block')return;"
   "tip.style.left=(e.clientX+12)+'px';"
   "tip.style.top=(e.clientY+12)+'px';});"
   "document.addEventListener('mouseout',function(e){"
   "var el=e.target;"
   "if(!el||!el.classList||!el.classList.contains('edgar2-ix'))return;"
   "tip.style.display='none';});}"
   "if(document.readyState==='loading')"
   "document.addEventListener('DOMContentLoaded',mark);"
   "else mark();"
   "})();</script>"))

(defn- inject-head
  "Insert `snippet` just before </head>, or at the top if missing."
  [html snippet]
  (if-let [idx (or (str/index-of html "</head>")
                    (str/index-of html "</HEAD>"))]
    (str (subs html 0 idx) snippet (subs html idx))
    (str snippet html)))

(defn- inject-ix-highlight
  "Insert highlight CSS/JS just before </head>, or at the top if missing."
  [html]
  (inject-head html ix-highlight-snippet))

(defn- html-escape
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- index-href
  "Index URL for a cached file at …/{cik}/{accession}/{doc}."
  [file]
  (let [accession (.getParentFile file)
        cik (when accession (.getParentFile accession))
        cik-name (some-> cik .getName not-empty)
        acc-name (some-> accession .getName not-empty)]
    (when (and cik-name acc-name)
      (str "/filing/" cik-name "/" acc-name "/index"))))

(defn- index-link-snippet
  [href]
  (str "<a id=\"edgar2-index\" href=\"" (html-escape href) "\" "
       "style=\"position:fixed;top:8px;left:8px;z-index:2147483647;"
       "background:#111;color:#fff;padding:4px 8px;"
       "font:12px/1.3 sans-serif;text-decoration:none\">"
       "Filing index</a>"))

(defn- inject-index-link
  "Insert the index link just after the opening body tag.
  An absolute href stays on this server when the filing sets a base URL."
  [html href]
  (let [snippet (index-link-snippet href)
        replaced (str/replace-first html #"(?i)<body\b[^>]*>"
                                    #(str % snippet))]
    (if (= replaced html)
      (str snippet html)
      replaced)))

(defn- inject-filing-chrome
  [html file]
  (let [html (-> html
                 (inject-head layout-snippet)
                 inject-ix-highlight)]
    (if-let [href (index-href file)]
      (inject-index-link html href)
      html)))

(defn- file-response
  [file]
  (cond
    (html-file? file)
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (inject-filing-chrome (slurp file :encoding "UTF-8") file)}

    (pdf-file? file)
    {:status 200
     :headers {"Content-Type" "application/pdf"
               "Content-Disposition" (str "inline; filename=\"" (.getName file) "\"")}
     :body file}

    :else
    {:status 200
     :headers {"Content-Type" (content-type file)}
     :body file}))
(defn- text-response
  [status body]
  {:status status
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body body})

(declare redirect)

(defn annual-report-file
  "Path of the latest 10-K, 20-F, or 40-F primary document.
  Downloads it into data/filings when it is not already there."
  [ticker]
  (util/ensure-identity!)
  (let [cik (util/resolve-filer ticker)
        form (ufiling/annual-form cik)
        f (e/filing cik :form form)
        primary (when f (ef/primary-doc (ef/filing-index f)))]
    (when primary
      (let [path (io/file ufiling/filing-dir
                          (str (:form f))
                          (str (:cik f))
                          (str (:accessionNumber f))
                          (str (:name primary)))]
        (if (.isFile path)
          path
          (when-let [saved (ef/filing-save! f ufiling/filing-dir)]
            (io/file saved)))))))

(defn annual-report-page
  "Hyper handler. Redirects to the cached primary document so relative
  image links in the filing resolve next to it."
  [req]
  (let [ticker (some-> (get-in req [:hyper/route :path-params :ticker])
                       str str/trim not-empty str/upper-case)]
    (try
      (if-let [file (and ticker (annual-report-file ticker))]
        (let [accession (.getParentFile file)
              cik (.getParentFile accession)]
          (redirect (str "/filing/" (.getName cik)
                         "/" (.getName accession)
                         "/" (.getName file))))
        (text-response 404 (str "No annual report for " ticker)))
      (catch Throwable e
        (text-response 502 (or (ex-message e) (str e)))))))

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

(defn- sec-miss?
  "EDGAR has no index at this CIK, or the index did not name a form."
  [e]
  (and (instance? clojure.lang.ExceptionInfo e)
       (or (http-404? e)
           (= :edgar.filing/not-found (:type (ex-data e))))))

(defn- accession-cik
  "Leading CIK of an accession number (the submitter, often a filing agent)."
  [accession]
  (second (re-find #"^(\d{10})-\d{2}-\d{6}$" (str accession))))

(defn- cik-dir-names
  "Padded and unpadded directory names used under data/filings."
  [cik]
  (when-let [s (some-> cik str str/trim not-empty)]
    (when (re-matches #"\d+" s)
      (let [n (parse-long s)]
        [(format "%010d" n) (str n)]))))

(defn- cached-doc
  "A document already stored under data/filings, or nil.
  Checks the route CIK and the accession-prefix CIK. Older saves used the
  submitter CIK taken from the accession number."
  [cik accession doc-name]
  (when (and accession doc-name)
    (let [names (distinct (mapcat cik-dir-names [cik (accession-cik accession)]))
          root (io/file ufiling/filing-dir)]
      (when (.isDirectory root)
        (some (fn [^java.io.File form-dir]
                (when (.isDirectory form-dir)
                  (some (fn [cik-name]
                          (let [f (io/file form-dir cik-name accession doc-name)]
                            (when (.isFile f) f)))
                        names)))
              (.listFiles root))))))

(defn- name-stem
  "Basename without `ext` (`html`, `xml`), or nil when the suffix differs."
  [name ext]
  (let [lower (str/lower-case (str name))
        suffix (str "." ext)]
    (when (str/ends-with? lower suffix)
      (subs lower 0 (- (count lower) (count suffix))))))

(defn- primary-name
  "Sequence-1 filename.

  XML forms list two sequence-1 rows. The first is a viewer label
  (`primary_doc.html`) whose archive link is `xsl…/primary_doc.xml`.
  That .html file is not in the accession. The .xml next to it is.
  A 10-K or 8-K has one sequence 1, so this returns that filename."
  [index]
  (let [names (->> (:files index)
                   (filter #(= "1" (str (:sequence %))))
                   (map :name)
                   (remove str/blank?)
                   vec)
        html-stems (set (keep #(name-stem % "html") names))]
    (or (some (fn [n]
                (when-let [stem (name-stem n "xml")]
                  (when (html-stems stem) n)))
              names)
        (first names))))

(defn- index-filing
  "Filing map from the archives index stored under `cik`."
  [cik accession]
  (let [cik (padded-cik cik)
        stub {:cik cik :accessionNumber accession}
        idx (ef/filing-index stub)
        form (:formType idx)]
    (if form
      (assoc stub
             :form form
             :primaryDocument (primary-name idx)
             :filingDate (:filingDate idx))
      (throw (ex-info "Could not determine form type from filing index"
                      {:type :edgar.filing/not-found
                       :accession-number accession
                       :cik cik})))))

(defn- load-filing
  "Filing map for `accession`.

  `cik` is the issuer from the route. The accession prefix is the submitter.
  For agent-filed documents (accession prefix 0001193125 is Donnelley) the
  files live under the issuer, and the submitter index often answers 503.
  edgarjure turns that into \"Filing not found\" and never tries the issuer.
  Try the issuer archives path first; a 5xx is retried on the other CIK and
  only then rethrown."
  [cik accession]
  (let [candidates (vec (->> [(padded-cik cik) (padded-cik (accession-cik accession))]
                             (remove nil?)
                             distinct))]
    (loop [i 0
           hard nil]
      (if (>= i (count candidates))
        (when hard (throw hard))
        (let [result (try
                       {:ok (index-filing (nth candidates i) accession)}
                       (catch clojure.lang.ExceptionInfo e
                         {:err e}))
              err (:err result)]
          (cond
            (:ok result) (:ok result)
            (sec-miss? err) (recur (inc i) hard)
            (< (inc i) (count candidates)) (recur (inc i) (or hard err))
            hard (throw hard)
            :else (throw err)))))))

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
    (io/file ufiling/filing-dir
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

(defn cached-primary
  "Path of the primary document named on `f`.
  Downloads it into data/filings when it is not already there.
  Uses `:primaryDocument` rather than `filing-save!`, which would ask the
  index again and pick the XML viewer label."
  [f]
  (when-let [doc (:primaryDocument f)]
    (when-let [path (cache-file f doc)]
      (if (.isFile path)
        path
        (download-doc! f doc path)))))

(defn- try-primary
  "Download `f`, or nil when SEC has no document at that CIK."
  [f]
  (try
    (cached-primary f)
    (catch clojure.lang.ExceptionInfo e
      (if (http-404? e) nil (throw e)))))

(defn- absolute-sec-url
  "Turn an Archives-relative href into an absolute https://www.sec.gov URL."
  [href]
  (cond
    (str/blank? href) nil
    (str/starts-with? href "https://") href
    (str/starts-with? href "http://") href
    (str/starts-with? href "/") (str "https://www.sec.gov" href)
    :else nil))

(defn- scanned-pdf-url
  "URL of the scanned paper PDF from the filing index, if listed.
  Newer paper filings link to /Archives/edgar/vprr/... rather than the
  accession directory, so the index href must be used — not filing-doc-url."
  [f]
  (try
    (let [html (ec/edgar-get (ef/filing-index-url f) :raw? true)
          href (or (second (re-find #"(?is)Scanned paper document</td>\s*<td[^>]*>\s*<a href=\"([^\"]+)\""
                                    html))
                   (second (re-find #"(?i)href=\"([^\"]+\.pdf)\"[^>]*>\s*scanned\.pdf"
                                    html)))]
      (absolute-sec-url href))
    (catch clojure.lang.ExceptionInfo e
      (if (http-404? e) nil (throw e)))))

(defn- download-url-bytes!
  [url path]
  (.mkdirs (.getParentFile path))
  (let [bytes (ec/edgar-get-bytes url)]
    (with-open [out (java.io.FileOutputStream. path)]
      (.write out ^bytes bytes)))
  path)

(defn- try-scanned-pdf
  "Cached scanned.pdf for a paper filing, or nil when the index has none."
  [f]
  (when-let [path (cache-file f "scanned.pdf")]
    (cond
      (.isFile path) path
      :else (when-let [url (scanned-pdf-url f)]
              (try
                (download-url-bytes! url path)
                (catch clojure.lang.ExceptionInfo e
                  (if (or (http-404? e)
                          (= 403 (:status (ex-data e))))
                    nil
                    (throw e))))))))

(defn- try-doc
  "Cached path of one named document, or nil when SEC has none at this CIK.
  Requests for .paper stubs prefer a scanned PDF when the index lists one."
  [f doc-name]
  (if (paper-filename? doc-name)
    (or (try-scanned-pdf f)
        (when-let [path (cache-file f doc-name)]
          (cond
            (.isFile path) path
            :else (try
                    (download-doc! f doc-name path)
                    (catch clojure.lang.ExceptionInfo e
                      (if (http-404? e) nil (throw e)))))))
    (when-let [path (cache-file f doc-name)]
      (cond
        (.isFile path) path
        (= "scanned.pdf" (str/lower-case (str doc-name)))
        (try-scanned-pdf f)
        :else (try
                (download-doc! f doc-name path)
                (catch clojure.lang.ExceptionInfo e
                  (if (http-404? e) nil (throw e))))))))

(defn- prefer-viewable
  "For paper stubs, prefer the scanned PDF when EDGAR has one."
  [f path]
  (if (and path (paper-filename? (.getName path)))
    (or (try-scanned-pdf f) path)
    path))

(defn raw-filing-file
  "Path of the primary document for an accession number.
  Downloads it into data/filings when it is not already there.

  `cik` is the issuer. The accession prefix is the submitter, which for
  agent-filed registration statements is a filing agent. Those documents
  are stored under the issuer CIK, so that path is tried first.
  Paper filings resolve to scanned.pdf when that scan is on the index."
  [cik accession]
  (util/ensure-identity!)
  (when-let [f (load-filing cik accession)]
    (let [issuer (padded-cik cik)
          issuer-filing (if issuer (assoc f :cik issuer) f)]
      (or (some->> (try-primary issuer-filing) (prefer-viewable issuer-filing))
          (when-not (same-cik? (:cik issuer-filing) (:cik f))
            (some->> (try-primary f) (prefer-viewable f)))))))

(defn filing-doc-file
  "Path of one document in a filing, such as an image beside the HTML.
  `cik` is the issuer. The accession prefix is the submitter, so the issuer
  path is tried first, same as `raw-filing-file`."
  [cik accession doc-name]
  (when-let [doc (safe-doc-name doc-name)]
    (or (when-not (paper-filename? doc)
          (cached-doc cik accession doc))
        (do
          (util/ensure-identity!)
          (when-let [f (load-filing cik accession)]
            (let [issuer (padded-cik cik)
                  issuer-filing (if issuer (assoc f :cik issuer) f)]
              (or (try-doc issuer-filing doc)
                  (when-not (same-cik? (:cik issuer-filing) (:cik f))
                    (try-doc f doc)))))))))

(defn- redirect
  [url]
  {:status 302
   :headers {"Location" url}
   :body ""})

(defn- filing-url
  [cik accession filename]
  (str "/filing/" (or (padded-cik cik) cik) "/" accession "/" filename))

(defn- index-row
  [cik accession {:keys [sequence description name type size]}]
  (let [href (when (safe-doc-name name)
               (filing-url cik accession name))
        label (html-escape name)]
    (str "<tr><td>" (html-escape sequence) "</td>"
         "<td>" (html-escape description) "</td>"
         "<td>" (html-escape type) "</td>"
         "<td>" (html-escape size) "</td>"
         "<td>" (if href
                  (str "<a href=\"" (html-escape href) "\">" label "</a>")
                  label)
         "</td></tr>")))

(defn- index-page-html
  [cik accession filing idx]
  (str "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/>"
       "<title>" (html-escape (or (:form filing) "Filing"))
       " " (html-escape accession) "</title>"
       "<style>"
       "body{font:14px/1.4 sans-serif;margin:24px}"
       "table{border-collapse:collapse}"
       "td,th{border:1px solid #ccc;padding:4px 8px;text-align:left}"
       "th{background:#f4f4f4}"
       "</style></head><body>"
       "<h1>" (html-escape (or (:formType idx) (:form filing) "Filing"))
       " " (html-escape (or (:filingDate idx) "")) "</h1>"
       "<p>" (html-escape accession) "</p>"
       "<table><thead><tr>"
       "<th>Seq</th><th>Description</th><th>Type</th><th>Size</th><th>Document</th>"
       "</tr></thead><tbody>"
       (str/join (map #(index-row cik accession %) (:files idx)))
       "</tbody></table></body></html>"))

(defn filing-index-page
  "Hyper handler. Lists every document on the SEC filing index.
  Each filename links to the document route, including the XBRL instance."
  [req]
  (let [cik (get-in req [:hyper/route :path-params :cik])
        accession (some-> (get-in req [:hyper/route :path-params :accession])
                          str str/trim not-empty)]
    (try
      (util/ensure-identity!)
      (if-let [f (and accession (load-filing cik accession))]
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (index-page-html cik accession f (ef/filing-index f))}
        (text-response 404 (str "No filing for " accession)))
      (catch clojure.lang.ExceptionInfo e
        (if (= :edgar.filing/not-found (:type (ex-data e)))
          (text-response 404 (or (ex-message e) "Filing not found"))
          (text-response 502 (or (ex-message e) (str e)))))
      (catch Throwable e
        (text-response 502 (or (ex-message e) (str e)))))))

(defn raw-filing-page
  "Hyper handler. Returns one filing document as a raw response.
  A request with no filename redirects to the primary document so relative
  image links in the HTML resolve next to it.
  The document name \"index\" is the filing index page.
  Paper stubs redirect to scanned.pdf when EDGAR lists a scan."
  [req]
  (let [cik (get-in req [:hyper/route :path-params :cik])
        accession (some-> (get-in req [:hyper/route :path-params :accession])
                          str str/trim not-empty)
        doc-name (some-> (get-in req [:hyper/route :path-params :doc])
                         str str/trim not-empty)
        doc (safe-doc-name doc-name)]
    (try
      (cond
        ;; A literal /index route conflicts with :doc in reitit. No SEC
        ;; document is named "index"; the archives index is {accession}-index.html.
        (and accession (= "index" doc-name))
        (filing-index-page req)

        (and accession doc)
        (if-let [file (filing-doc-file cik accession doc)]
          (if (not= (str/lower-case doc) (str/lower-case (.getName file)))
            (redirect (filing-url cik accession (.getName file)))
            (file-response file))
          (text-response 404 (str "No document " doc " in " accession)))

        accession
        (if-let [file (raw-filing-file cik accession)]
          (redirect (filing-url cik accession (.getName file)))
          (text-response 404 (str "No filing for " accession)))

        :else
        (text-response 404 "No filing"))
      (catch clojure.lang.ExceptionInfo e
        (if (= :edgar.filing/not-found (:type (ex-data e)))
          (text-response 404 (or (ex-message e) "Filing not found"))
          (text-response 502 (or (ex-message e) (str e)))))
      (catch Throwable e
        (text-response 502 (or (ex-message e) (str e)))))))
