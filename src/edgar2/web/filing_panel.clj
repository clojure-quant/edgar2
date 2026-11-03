(ns edgar2.web.filing-panel
  "Right-hand XBRL facts panel for HTML filing documents.

  A filing carries arbitrary CSS, so the table is served as a separate
  same-origin document in an iframe. Rows come from the cached filing file
  already resolved for the page, not from a second SEC request. The
  taxonomy combobox submits a GET request, so switching taxonomies reuses
  the cached rows instead of re-parsing the filing."
  (:require [clojure.string :as str]
            [edgar2.filing.xbrl-overview :as xbrl-overview]
            [edgar2.filing.xbrl-loader :as xbrl-loader]))

(defn fact-rows
  "Every fact from `docs`, the vector built by `xbrl-loader/load-documents`.

  Keeps document order and duplicate facts, like the demo table, and keeps
  only the projected `:taxonomy`, `:concept`, `:value`, `:period`, `:cik`,
  and `:dimension` keys. `:cik` is the fact's context entity identifier as
  written in the document, not the filing's CIK.

  Facts whose context does not resolve, or whose context has no entity, have
  nil period/cik/dimension."
  [docs]
  (->> (xbrl-overview/facts-with-concepts (xbrl-overview/overview docs))
       (mapv #(assoc (select-keys % [:taxonomy :concept :value :period])
                     :cik (get-in % [:entity :cik])
                     :dimension (get-in % [:entity :dimension])))))

(defn facts->taxonomies
  "Distinct `:taxonomy` values in `facts`, `:dei` first, then by name.

  Facts without a taxonomy are ignored: their qname has no prefix, so there
  is nothing the combobox could select."
  [facts]
  (->> facts
       (keep :taxonomy)
       distinct
       (sort-by (fn [t] [(if (= :dei t) 0 1) (name t)]))
       vec))

(defn rows-for-taxonomy
  "`rows` belonging to `taxonomy`. A nil taxonomy has no rows."
  [rows taxonomy]
  (if taxonomy
    (filterv #(= taxonomy (:taxonomy %)) rows)
    []))

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

(def ^:private max-inline-value 50)

(defn- truncated-value-html
  "Clickable shortened value plus its full text in a hidden sibling span.
  The script copies the span's `textContent`, so nothing is re-escaped in JS."
  [s]
  (str "<span class=\"fact-more\" role=\"button\" tabindex=\"0\""
       " aria-expanded=\"false\">"
       (html-escape (str (str/trim (subs s 0 max-inline-value)) "..."))
       "</span>"
       "<span class=\"fact-full\" hidden>"
       (html-escape s)
       "</span>"))

(defn- value-html
  "Escaped value cell. Only non-numeric strings longer than
  `max-inline-value` become a truncated, clickable trigger."
  [value]
  (let [s (label value)]
    (if (and (string? value) (> (count s) max-inline-value))
      (truncated-value-html s)
      (html-escape s))))

(defn- period-html
  "Escaped duration `start-end` or instant date; an absent period becomes
  the shared em dash so the fact stays visible."
  [period]
  (html-escape (label (xbrl-overview/printable-period period))))

(defn- qname
  "Keyword as `prefix:name`, without the leading colon `str` would add.
  A typed member is already a plain string."
  [v]
  (if (keyword? v)
    (if-let [ns (namespace v)]
      (str ns ":" (name v))
      (name v))
    (str v)))

(defn- dimension-title
  "Axis/member pairs for the hover title, e.g.
  `us-gaap:SegmentsAxis = us-gaap:RetailMember`."
  [dimension]
  (str/join ", " (map (fn [[axis member]]
                        (str (qname axis) " = " (qname member)))
                      dimension)))

(defn- dimension-html
  "Visible member text from `xbrl-overview/printable-dimension`, with the axis/member
  pairs in a `title`. An absent dimension is an em dash with no title, and
  both the text and the title are escaped."
  [dimension]
  (let [s (xbrl-overview/printable-dimension dimension)]
    (if s
      (str "<span class=\"dimension\" title=\""
           (html-escape (dimension-title dimension)) "\">"
           (html-escape s) "</span>")
      (html-escape (label nil)))))

(defn- requested-taxonomy
  "`:dei` from a query value such as `\"dei\"`, `\":dei\"`, or `\" dei \"`."
  [raw]
  (let [s (some-> raw str str/trim)]
    (when-not (str/blank? s)
      (keyword (str/replace s #"^:" "")))))

(defn- selected-taxonomy
  "The requested taxonomy when it exists, otherwise `:dei`, otherwise the
  first available taxonomy.

  Checking against `taxonomies` first also rejects arbitrary input from the
  URL, which must never reach `rows-for-taxonomy` unchecked."
  [taxonomies raw]
  (let [requested (requested-taxonomy raw)]
    (cond
      (some #{requested} taxonomies) requested
      (some #{:dei} taxonomies) :dei
      :else (first taxonomies))))

(defn rows-html
  "Table body rows for `rows`. Every cell is escaped; a long string value
  renders shortened with its full text in a hidden sibling span."
  [rows]
  (str/join
   (map (fn [{:keys [taxonomy concept period value cik dimension]}]
          (str "<tr><td>" (html-escape (label taxonomy)) "</td>"
               "<td class=\"concept\">" (html-escape (label concept)) "</td>"
               "<td class=\"period\">" (period-html period) "</td>"
               "<td>" (value-html value) "</td>"
               "<td class=\"cik\">" (html-escape (or cik "")) "</td>"
               "<td>" (dimension-html dimension) "</td></tr>"))
        rows)))

(defn- taxonomy-html
  "Combobox above the table. The browser URL-encodes the selected value."
  [taxonomies selected]
  (str "<form class=\"taxonomy\" method=\"get\" action=\"\">"
       "<label for=\"taxonomy\">Taxonomy</label>"
       "<select id=\"taxonomy\" name=\"taxonomy\""
       " onchange=\"this.form.submit()\">"
       (str/join
        (map (fn [t]
               (str "<option value=\"" (html-escape (name t)) "\""
                    (when (= t selected) " selected")
                    ">" (html-escape (name t)) "</option>"))
             taxonomies))
       "</select>"
       "<noscript><button type=\"submit\">Show</button></noscript>"
       "</form>"))

(defonce ^:private rows-cache
  (atom {}))

(defn- cache-key
  "Path, mtime, and length: a re-downloaded filing must not reuse old rows."
  [^java.io.File f]
  [(.getPath f) (.lastModified f) (.length f)])

(defn- data-for-file
  "All fact rows and their taxonomies for `file`, parsed once per
  path/mtime/length. Filtering by taxonomy happens on the cached rows."
  [file]
  (let [k (cache-key file)]
    (if-let [data (get @rows-cache k)]
      data
      (let [rows (fact-rows (xbrl-loader/load-documents file))
            data {:rows rows :taxonomies (facts->taxonomies rows)}]
        (swap! rows-cache assoc k data)
        data))))

(def ^:private page-style
  (str "<style>"
       "html,body{height:100%}"
       "body{margin:0;display:flex;flex-direction:column;"
       "font:13px/1.4 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;"
       "color:#111;background:#fff}"
       "h1{flex:0 0 auto;margin:0;padding:10px 12px;font-size:13px;"
       "font-weight:600;border-bottom:1px solid #ddd;"
       "white-space:nowrap;overflow:hidden;text-overflow:ellipsis}"
       "form.taxonomy{flex:0 0 auto;display:flex;align-items:center;gap:6px;"
       "padding:8px 12px;border-bottom:1px solid #ddd}"
       "form.taxonomy select{font:inherit;max-width:100%}"
       ".table-wrap{flex:1 1 auto;overflow:auto}"
       "table{border-collapse:collapse;width:100%}"
       "th,td{border-bottom:1px solid #eee;padding:4px 8px;text-align:left;"
       "vertical-align:top}"
       "th{position:sticky;top:0;background:#f4f4f4;font-weight:600;z-index:1}"
       "td:first-child{color:#555;white-space:nowrap}"
       "td.concept{overflow-wrap:anywhere}"
       "td.period,td.cik{white-space:nowrap}"
       ".dimension{display:inline-block;max-width:8em;overflow:hidden;"
       "text-overflow:ellipsis;white-space:nowrap;vertical-align:bottom}"
       ".empty,.error{margin:12px;color:#555}"
       ".error{color:#a00;white-space:pre-wrap}"
       ".fact-more{cursor:pointer;color:#06c;text-decoration:underline dotted;"
       "overflow-wrap:anywhere}"
       ".fact-full{display:none}"
       ".fact-popup{position:fixed;z-index:10;background:#fff;color:#111;"
       "border:1px solid #bbb;padding:8px 10px;"
       "box-shadow:0 2px 8px rgba(0,0,0,.25);"
       "max-width:min(480px,calc(100vw - 16px));max-height:60vh;overflow:auto;"
       "white-space:pre-wrap;overflow-wrap:anywhere}"
       "</style>"))

(def ^:private panel-script
  "Popup for shortened fact values. One fixed-position div is created on
  load; content always comes from the hidden span's `textContent`."
  (str "<script>(function(){"
       "function init(){"
       "var popup=document.createElement('div');"
       "popup.id='edgar2-fact-popup';"
       "popup.className='fact-popup';"
       "popup.hidden=true;"
       "popup.tabIndex=-1;"
       "(document.body||document.documentElement).appendChild(popup);"
       "var open=null;"
       "function close(){"
       "if(!open)return;"
       "open.setAttribute('aria-expanded','false');"
       "open=null;"
       "popup.hidden=true;"
       "popup.textContent='';}"
       "function place(t){"
       "var r=t.getBoundingClientRect();"
       "popup.style.left='0px';popup.style.top='0px';"
       "var pw=popup.offsetWidth,ph=popup.offsetHeight;"
       "var vw=window.innerWidth,vh=window.innerHeight;"
       "var left=Math.max(8,Math.min(r.left,vw-pw-8));"
       "var top=r.bottom+6;"
       "if(top+ph>vh-8)top=Math.max(8,r.top-ph-6);"
       "popup.style.left=left+'px';"
       "popup.style.top=top+'px';}"
       "function show(t){"
       "if(open===t){close();return;}"
       "close();"
       "var full=t.parentNode?"
       "t.parentNode.querySelector('.fact-full'):null;"
       "popup.textContent=full?full.textContent:'';"
       "popup.hidden=false;"
       "t.setAttribute('aria-expanded','true');"
       "open=t;"
       "place(t);}"
       "document.addEventListener('click',function(e){"
       "var t=e.target&&e.target.closest?"
       "e.target.closest('.fact-more'):null;"
       "if(t){e.preventDefault();show(t);return;}"
       "if(open&&popup.contains(e.target))return;"
       "close();});"
       "document.addEventListener('keydown',function(e){"
       "var t=e.target&&e.target.closest?"
       "e.target.closest('.fact-more'):null;"
       "if(t&&(e.key==='Enter'||e.key===' '||e.key==='Spacebar')){"
       "e.preventDefault();show(t);return;}"
       "if(e.key==='Escape'&&open){"
       "var o=open;close();if(o.focus)o.focus();}});"
       "document.addEventListener('scroll',close,true);"
       "window.addEventListener('resize',close);}"
       "if(document.readyState==='loading')"
       "document.addEventListener('DOMContentLoaded',init);"
       "else init();"
       "})();</script>"))

(defn page-html
  "Standalone HTML document for the panel iframe.

  A parse failure is reported inside the panel so the enclosing filing page
  still renders. `file` is the cached filing document and `requested` is the
  raw `taxonomy` query value; an unknown value falls back to `:dei`, then to
  the first available taxonomy."
  ([file]
   (page-html file nil))
  ([file requested]
   (let [result (try
                  {:data (data-for-file file)}
                  (catch Throwable e
                    {:error (or (ex-message e) (str e))}))]
     (str "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/>"
          "<title>XBRL facts</title>" page-style "</head><body>"
          (if-let [{:keys [rows taxonomies]} (:data result)]
            (let [selected (selected-taxonomy taxonomies requested)
                  shown (rows-for-taxonomy rows selected)]
              (str "<h1>XBRL facts (" (count shown) ")</h1>"
                   (when (seq taxonomies)
                     (taxonomy-html taxonomies selected))
                   "<div class=\"table-wrap\">"
                   (if (seq shown)
                      (str "<table><thead><tr>"
                           "<th>Taxonomy</th><th>Concept</th><th>Period</th>"
                           "<th>Value</th><th>CIK</th><th>Dimension</th>"
                           "</tr></thead><tbody>"
                          (rows-html shown)
                          "</tbody></table>")
                     "<p class=\"empty\">No XBRL facts in this document.</p>")
                   "</div>"))
            (str "<h1>XBRL facts</h1>"
                 "<div class=\"table-wrap\"><p class=\"error\">"
                 (html-escape (str "Could not read XBRL facts: " (:error result)))
                 "</p></div>"))
          panel-script "</body></html>"))))

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
       "\" title=\"XBRL facts panel\"></iframe>"
       "<div id=\"edgar2-xbrl-resizer\" role=\"separator\""
       " aria-orientation=\"vertical\""
       " aria-label=\"Resize XBRL facts panel\"></div>"))
