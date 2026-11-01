(ns edgar2.web.filing-panel
  "Right-hand DEI facts panel for HTML filing documents.

  A filing carries arbitrary CSS, so the table is served as a separate
  same-origin document in an iframe. Rows come from the cached filing file
  already resolved for the page, not from a second SEC request."
  (:require [clojure.string :as str]
            [edgar2.filing.xbrl :as xbrl]
            [edgar2.filing.xbrl-loader :as xbrl-loader]))

(defn dei-rows
  "DEI facts from `docs`, the vector built by `xbrl-loader/load-documents`.

  Keeps document order and duplicate facts, like the demo table, and keeps
  only the projected `:taxonomy`, `:concept`, and `:value` keys."
  [docs]
  (->> (:facts (xbrl/overview docs))
       (filter #(= :dei (:taxonomy %)))
       (mapv #(select-keys % [:taxonomy :concept :value]))))

(defn- html-escape
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- label
  "Printable cell text. Keywords lose their leading colon, and a nil or
  blank value becomes an em dash so the fact is still visible."
  [v]
  (cond
    (nil? v) "\u2014"
    (keyword? v) (name v)
    :else (let [s (str/trim (str v))]
            (if (str/blank? s) "\u2014" s))))

(defn rows-html
  "Table body rows for `rows`. Every cell is escaped."
  [rows]
  (str/join
   (map (fn [{:keys [taxonomy concept value]}]
          (str "<tr><td>" (html-escape (label taxonomy)) "</td>"
               "<td>" (html-escape (label concept)) "</td>"
               "<td>" (html-escape (label value)) "</td></tr>"))
        rows)))

(defonce ^:private rows-cache
  (atom {}))

(defn- cache-key
  "Path, mtime, and length: a re-downloaded filing must not reuse old rows."
  [^java.io.File f]
  [(.getPath f) (.lastModified f) (.length f)])

(defn- rows-for-file
  "DEI rows for `file`, parsed once per path/mtime/length."
  [file]
  (let [k (cache-key file)]
    (if-let [rows (get @rows-cache k)]
      rows
      (let [rows (dei-rows (xbrl-loader/load-documents file))]
        (swap! rows-cache assoc k rows)
        rows))))

(def ^:private page-style
  (str "<style>"
       "html,body{height:100%}"
       "body{margin:0;display:flex;flex-direction:column;"
       "font:13px/1.4 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;"
       "color:#111;background:#fff}"
       "h1{flex:0 0 auto;margin:0;padding:10px 12px;font-size:13px;"
       "font-weight:600;border-bottom:1px solid #ddd;"
       "white-space:nowrap;overflow:hidden;text-overflow:ellipsis}"
       ".table-wrap{flex:1 1 auto;overflow:auto}"
       "table{border-collapse:collapse;width:100%}"
       "th,td{border-bottom:1px solid #eee;padding:4px 8px;text-align:left;"
       "vertical-align:top}"
       "th{position:sticky;top:0;background:#f4f4f4;font-weight:600;z-index:1}"
       "td:first-child{color:#555;white-space:nowrap}"
       "td:nth-child(2){overflow-wrap:anywhere}"
       ".empty,.error{margin:12px;color:#555}"
       ".error{color:#a00;white-space:pre-wrap}"
       "</style>"))

(defn page-html
  "Standalone HTML document for the panel iframe.

  A parse failure is reported inside the panel so the enclosing filing page
  still renders. `file` is the cached filing document."
  [file]
  (let [result (try
                 {:rows (rows-for-file file)}
                 (catch Throwable e
                   {:error (or (ex-message e) (str e))}))]
    (str "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/>"
         "<title>XBRL DEI facts</title>" page-style "</head><body>"
         (if-let [rows (:rows result)]
           (str "<h1>XBRL DEI facts (" (count rows) ")</h1>"
                "<div class=\"table-wrap\">"
                (if (seq rows)
                  (str "<table><thead><tr>"
                       "<th>Taxonomy</th><th>Concept</th><th>Value</th>"
                       "</tr></thead><tbody>"
                       (rows-html rows)
                       "</tbody></table>")
                  "<p class=\"empty\">No DEI facts in this document.</p>")
                "</div>")
           (str "<h1>XBRL DEI facts</h1>"
                "<div class=\"table-wrap\"><p class=\"error\">"
                (html-escape (str "Could not read XBRL facts: " (:error result)))
                "</p></div>"))
         "</body></html>")))

(defn head-snippet
  "CSS and drag script for the panel, injected into the filing's head.

  `layout-snippet` supplies a 0px fallback; this snippet comes later so its
  320px default wins. The resizer disables pointer events on the iframe while
  dragging, otherwise mousemove stops at the frame boundary and the drag
  sticks."
  []
  (str "<style id=\"edgar2-xbrl-panel-style\">"
       ":root{--edgar2-panel-w:320px}"
       "body{width:auto!important;overflow-x:auto!important}"
       "#edgar2-xbrl-panel{position:fixed;top:0;right:0;"
       "width:var(--edgar2-panel-w);min-width:0;height:100vh;"
       "z-index:2147483646;background:#fff;border:0;"
       "border-left:1px solid #bbb;display:block!important}"
       "#edgar2-xbrl-resizer{position:fixed;top:0;bottom:0;"
       "right:calc(var(--edgar2-panel-w) - 3px);width:6px;"
       "z-index:2147483647;cursor:col-resize;background:transparent;"
       "touch-action:none}"
       "</style>"
       "<script>(function(){"
       "function init(){"
       "var root=document.documentElement;"
       "var panel=document.getElementById('edgar2-xbrl-panel');"
       "var grip=document.getElementById('edgar2-xbrl-resizer');"
       "if(!panel||!grip)return;"
       "var dragging=false;"
       "function clamp(w){"
       "var max=Math.max(160,Math.floor(window.innerWidth*0.8));"
       "return Math.max(160,Math.min(max,w));}"
       "function setWidth(w){"
       "root.style.setProperty('--edgar2-panel-w',clamp(w)+'px');}"
       "grip.addEventListener('mousedown',function(e){"
       "dragging=true;e.preventDefault();"
       "panel.style.pointerEvents='none';"
       "document.body.style.userSelect='none';"
       "document.body.style.cursor='col-resize';});"
       "document.addEventListener('mousemove',function(e){"
       "if(!dragging)return;setWidth(window.innerWidth-e.clientX);});"
       "function stop(){"
       "if(!dragging)return;dragging=false;"
       "panel.style.pointerEvents='';"
       "document.body.style.userSelect='';"
       "document.body.style.cursor='';}"
       "document.addEventListener('mouseup',stop);"
       "window.addEventListener('blur',stop);"
       "window.addEventListener('resize',function(){"
       "var w=parseFloat(getComputedStyle(root)"
       ".getPropertyValue('--edgar2-panel-w'))||320;"
       "setWidth(w);});}"
       "if(document.readyState==='loading')"
       "document.addEventListener('DOMContentLoaded',init);"
       "else init();"
       "})();</script>"))

(defn body-snippet
  "Iframe and drag grip inserted just after the filing's opening body tag."
  [panel-url]
  (str "<iframe id=\"edgar2-xbrl-panel\" src=\"" (html-escape panel-url)
       "\" title=\"XBRL DEI facts\"></iframe>"
       "<div id=\"edgar2-xbrl-resizer\" role=\"separator\""
       " aria-orientation=\"vertical\""
       " aria-label=\"Resize XBRL DEI facts panel\"></div>"))
