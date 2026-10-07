(ns edgar2.description
  "Company descriptions from Nasdaq, Finviz, and MarketBeat.

  Each site writes its own blurb, so the three texts are not the same."
  (:require [clojure.string :as str]
            [jsonista.core :as json])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]))

(def user-agent
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36")

(defn http-client
  []
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.connectTimeout (Duration/ofSeconds 15))
      (.build)))

(defn request
  [url headers]
  (let [b (-> (HttpRequest/newBuilder)
              (.uri (URI/create url))
              (.timeout (Duration/ofSeconds 20))
              (.header "User-Agent" user-agent)
              (.GET))]
    (.build (reduce (fn [b [k v]] (.header b (str k) (str v))) b headers))))

(defn http-get
  [client url headers]
  (let [resp (.send client
                    (request url headers)
                    (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode resp)
     :body (.body resp)}))

(defn snippet
  [s]
  (let [s (str s)]
    (subs s 0 (min 240 (count s)))))

(defn as-ticker
  [ticker]
  (cond
    (string? ticker) ticker
    (instance? clojure.lang.Named ticker) (name ticker)
    :else (str ticker)))

(defn normalize-ticker
  [ticker]
  (let [t (-> ticker as-ticker str/trim str/upper-case)]
    (when (str/blank? t)
      (throw (ex-info "ticker is required" {})))
    t))

(defn unescape-html
  [s]
  (-> s
      (str/replace "&amp;" "&")
      (str/replace "&nbsp;" " ")
      (str/replace "&quot;" "\"")
      (str/replace "&apos;" "'")
      (str/replace "&#39;" "'")
      (str/replace "&lt;" "<")
      (str/replace "&gt;" ">")
      (str/replace #"&#(\d+);"
                   (fn [[_ n]] (str (char (Integer/parseInt n)))))
      (str/replace #"&#x([0-9A-Fa-f]+);"
                   (fn [[_ n]] (str (char (Integer/parseInt n 16)))))))

(defn plain
  [s]
  (-> (str s)
      (str/replace #"<[^>]+>" "")
      unescape-html
      (str/replace #"\s+" " ")
      str/trim
      not-empty))

(defn read-json
  [body]
  (json/read-value body json/keyword-keys-object-mapper))

(defn nasdaq-description
  [client ticker]
  (let [url (str "https://api.nasdaq.com/api/company/" ticker "/company-profile")
        page (str "https://www.nasdaq.com/market-activity/stocks/" (str/lower-case ticker) "/company-profile")
        {:keys [status body]} (http-get client url {"Accept" "application/json"})]
    (when-not (= 200 status)
      (throw (ex-info (str "Nasdaq profile failed: HTTP " status)
                      {:status status :body (snippet body)})))
    {:url page
     :text (plain (get-in (read-json body) [:data :CompanyDescription :value]))}))

(defn finviz-description
  [client ticker]
  (let [url (str "https://finviz.com/stock?t=" ticker)
        {:keys [status body]} (http-get client url {})
        bio (second (re-find #"(?s)quote_profile-bio\">(.*?)</div>" (str body)))]
    (when-not (= 200 status)
      (throw (ex-info (str "Finviz profile failed: HTTP " status)
                      {:status status :body (snippet body)})))
    (when-not bio
      (throw (ex-info "Finviz profile bio not found" {:status status})))
    {:url url
     :text (plain bio)}))

(defn marketbeat-exchange
  "MarketBeat path segment for a Nasdaq quote-info exchange code."
  [exchange]
  (let [x (str/upper-case (str exchange))]
    (cond
      (str/blank? x) nil
      (str/includes? x "NASDAQ") "NASDAQ"
      (or (str/includes? x "AMEX") (str/includes? x "AMERICAN")) "NYSEAMERICAN"
      (str/includes? x "NYSE") "NYSE"
      (str/includes? x "OTC") "OTCMKTS"
      :else nil)))

(defn nasdaq-exchange
  [client ticker]
  (let [url (str "https://api.nasdaq.com/api/quote/" ticker "/info?assetclass=stocks")
        {:keys [status body]} (http-get client url {"Accept" "application/json"})]
    (when-not (= 200 status)
      (throw (ex-info (str "Nasdaq quote failed: HTTP " status)
                      {:status status :body (snippet body)})))
    (marketbeat-exchange (get-in (read-json body) [:data :exchange]))))

(defn disclaimer?
  [s]
  (boolean (re-find #"(?i)^AI Generated\b" (str s))))

(defn marketbeat-overview
  [html]
  (when-let [block (second (re-find #"(?s)id=\"overview\">(.*?)</div>" (str html)))]
    (let [paras (->> (re-seq #"(?s)<p[^>]*>(.*?)</p>" block)
                     (map second)
                     (keep plain)
                     (remove disclaimer?))]
      (when (seq paras)
        (str/join "\n\n" paras)))))

(defn marketbeat-description
  [client ticker]
  (let [exchange (or (nasdaq-exchange client ticker)
                     (throw (ex-info "No MarketBeat exchange for ticker" {:ticker ticker})))
        url (str "https://www.marketbeat.com/stocks/" exchange "/" ticker "/")
        {:keys [status body]} (http-get client url {"Accept" "text/html"})]
    (when-not (= 200 status)
      (throw (ex-info (str "MarketBeat profile failed: HTTP " status)
                      {:status status :url url :body (snippet body)})))
    {:url url
     :text (marketbeat-overview body)}))

(def sources
  [{:source :nasdaq :fetch nasdaq-description}
   {:source :finviz :fetch finviz-description}
   {:source :marketbeat :fetch marketbeat-description}])

(def preferred-sources
  "Fallback order for a single blurb: MarketBeat, then Finviz, then Nasdaq."
  [{:source :marketbeat :fetch marketbeat-description}
   {:source :finviz :fetch finviz-description}
   {:source :nasdaq :fetch nasdaq-description}])

(defn fetch-source
  [client ticker {:keys [source fetch]}]
  (try
    (let [{:keys [url text]} (fetch client ticker)]
      (if text
        {:source source :url url :text text}
        {:source source :url url :text nil :error "empty description"}))
    (catch Exception e
      {:source source :url nil :text nil :error (.getMessage e)})))

(defn preferred
  "First company description that has text.

  Tries MarketBeat, then Finviz, then Nasdaq. Returns
  `{:source :url :text}`, or nil when every site fails."
  [ticker]
  (let [t (normalize-ticker (if (map? ticker) (:ticker ticker) ticker))
        client (http-client)]
    (some (fn [source]
            (let [{:keys [text] :as row} (fetch-source client t source)]
              (when text row)))
          preferred-sources)))

(defn description
  "Download company descriptions for a ticker from Nasdaq, Finviz, and MarketBeat.

  `ticker` is a symbol string, or `{:ticker MSFT}` from `clj -X:description`.
  Returns `{:ticker \"MSFT\" :descriptions [{:source :url :text} ...]}`."
  [ticker]
  (let [t (normalize-ticker (if (map? ticker) (:ticker ticker) ticker))
        client (http-client)
        result {:ticker t
                :descriptions (mapv #(fetch-source client t %) sources)}]
    (println t)
    (doseq [{:keys [source url text error]} (:descriptions result)]
      (println)
      (println (str (name source) "  " url))
      (println (or text (str "error: " error))))
    result))
