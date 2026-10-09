(ns edgar2.web.ticker
  "Company ticker kept in Hyper tab state.

  Tab state survives navigation between /company pages in the same browser
  tab. A ?ticker= query param seeds an empty tab, so a shared link or a new
  tab still opens the right company."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [hyper.effects :as effects])
  (:import [java.net URLEncoder]))

(defn normalize
  [s]
  (some-> s str str/trim not-empty str/upper-case))

(defn ticker*
  "Cursor for the company ticker on this browser tab."
  []
  (h/tab-cursor :company/ticker))

(defn current
  []
  (normalize @(ticker*)))

(defn query
  "Query params that carry the current ticker, or nil when none is set."
  []
  (when-let [t (current)]
    {:ticker t}))

(defn- request-ticker
  [req]
  (let [q (get-in req [:hyper/route :query-params])]
    (normalize (or (:ticker q) (get q "ticker")))))

(defn seed!
  "If this tab has no ticker yet, take one from ?ticker=.
  Returns the tab's ticker after seeding."
  [req]
  (when-let [from-url (request-ticker req)]
    (let [t* (ticker*)]
      (when (nil? @t*)
        (reset! t* from-url))))
  (current))

(defn company-link
  "Attributes for an `<a>` that opens company financials for `ticker`
  and stores it on the tab."
  [ticker]
  (let [ticker (normalize ticker)]
    {:href (str "/company/financials?ticker=" (URLEncoder/encode ticker "UTF-8"))
     :data-on:click__prevent
     (h/action
       (reset! (ticker*) ticker)
       (effects/navigate! :company-financials {} {:ticker ticker}))}))

(defn watch!
  "Call `on-ticker` with the normalized ticker now and whenever it changes.
  Returns a function that removes the watch. Call from a view `:mount`.

  A cursor write during mount lands in the render overlay and is copied onto
  the tab a moment later. That copy is ignored when it repeats the value
  already delivered."
  [on-ticker]
  (let [t* (ticker*)
        k (Object.)
        seen (atom (normalize @t*))]
    (add-watch t* k
               (fn [_ _ _ new]
                 (let [new (normalize new)]
                   (when-not (= new @seen)
                     (reset! seen new)
                     (on-ticker new)))))
    (on-ticker @seen)
    (fn [] (remove-watch t* k))))

(defn load!
  "Fetch on a background thread and store `:loading`, then `:ready` or `:error`,
  on `result*`. `gen` is an atom. A newer `load!` or a cleared ticker drops
  this result, and so does `alive?` turning false."
  [result* alive? gen ticker fetch]
  (let [g (swap! gen inc)]
    (reset! result* {:status :loading :ticker ticker})
    (future
      (let [next (try
                   {:status :ready :ticker ticker :data (fetch)}
                   (catch Throwable e
                     {:status :error
                      :ticker ticker
                      :message (or (ex-message e) (str e))}))]
        (when (and @alive? (= g @gen))
          (reset! result* next)))))
  nil)

(defn begin
  "Load whenever the tab ticker changes. `fetch` is `(fn [ticker] data)`.
  A missing ticker is the empty state. Returns `{:result* :stop}` for a
  form-3 view. Call from `:mount` after `seed!`."
  [fetch]
  (let [result* (atom {:status :idle})
        alive? (atom true)
        gen (atom 0)
        stop (watch!
              (fn [t]
                (if-not t
                  (do (swap! gen inc)
                      (reset! result* {:status :empty}))
                  (load! result* alive? gen t #(fetch t)))))]
    (h/watch! result*)
    {:result* result*
     :stop (fn []
             (reset! alive? false)
             (stop))}))
