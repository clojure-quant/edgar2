(ns edgar2.web.ingress
  "Status of the ingress datasets, and a button that runs whatever is due."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.report.ingress :as ingress]
            [edgar2.web.nav :refer [nav]])
  (:import [java.io Writer]))

(defonce job*
  (atom {:status :idle
         :lines []
         :plan nil
         :current nil
         :finished #{}
         :error nil}))

(defonce running* (atom false))

(defn- consume-text
  [buf on-line s]
  (locking buf
    (.append buf (str s))
    (loop []
      (let [text (str buf)
            i (.indexOf text "\n")]
        (when (>= i 0)
          (on-line (subs text 0 i))
          (.setLength buf 0)
          (.append buf (subs text (inc i)))
          (recur))))))

(defn- line-writer
  "Writer that calls `on-line` once per line. Clojure prints with
  Writer.write and Writer.append, so both entry points feed the same buffer."
  [on-line]
  (let [buf (StringBuilder.)
        text-of (fn [x]
                  (cond
                    (string? x) x
                    (instance? CharSequence x) (str x)
                    (and x (.isArray (class x))) (String. ^chars x)
                    (integer? x) (str (char x))
                    :else (str x)))]
    (proxy [Writer] []
      (write
        ([x]
         (consume-text buf on-line (text-of x)))
        ([x off len]
         (let [s (text-of x)]
           (consume-text buf on-line (subs s off (+ off len))))))
      (append
        ([x]
         (consume-text buf on-line (text-of x))
         this)
        ([x start end]
         (let [s (text-of x)]
           (consume-text buf on-line (subs s start end))
           this)))
      (flush []
        (locking buf
          (when (pos? (.length buf))
            (on-line (str buf))
            (.setLength buf 0))))
      (close [] (.flush this)))))

(defn start!
  "Run ingress on a background thread. Further clicks do nothing until it finishes."
  []
  (when (compare-and-set! running* false true)
    (reset! job* {:status :running
                  :lines []
                  :plan (ingress/plan)
                  :current nil
                  :finished #{}
                  :error nil})
    (future
      (let [lines (atom [])
            flushed-at (atom 0)
            publish! (fn []
                       (reset! flushed-at (System/currentTimeMillis))
                       (swap! job* assoc :lines (vec (take-last 80 @lines))))
            on-line (fn [line]
                      (swap! lines conj line)
                      (when (> (- (System/currentTimeMillis) @flushed-at) 400)
                        (publish!)))]
        (try
          (binding [*out* (line-writer on-line)
                    *flush-on-newline* true
                    ingress/*step* (fn [{:keys [event id]}]
                                     (case event
                                       :start (swap! job* assoc :current id)
                                       :done (do
                                               (swap! job* update :finished (fnil conj #{}) id)
                                               (publish!))
                                       nil))]
            (ingress/run {}))
          (publish!)
          (swap! job* assoc :status :done :current nil :error nil)
          (catch Throwable t
            (on-line (str "ERROR " (or (ex-message t) (.getMessage t) t)))
            (publish!)
            (swap! job* assoc :status :error
                   :current nil
                   :error (or (ex-message t) (.getMessage t) (str t))))
          (finally
            (try
              (swap! job* assoc :plan (ingress/plan))
              (catch Throwable _))
            (reset! running* false)))))
    nil))

(defn- as-of
  [{:keys [id date quarter]}]
  (case id
    :fsds (cond
            (and quarter date) (str quarter " · " date)
            quarter quarter
            date date
            :else "—")
    :stats "—"
    (or date "—")))

(defn- state-label
  [item job]
  (cond
    (= (:id item) (:current job)) "running"
    (and (= :running (:status job)) (contains? (:finished job) (:id item))) "done"
    (:due? item) "needs download"
    :else "current"))

(defn- page
  [req]
  (let [job @job*
        running? (= :running (:status job))
        items (if running? (:plan job) (ingress/plan))
        due? (some :due? items)]
    [:div
     (nav req)
     [:h1 "Ingress"]
     [:p "Prices refresh after 3 days (newest session). Ticker exchange and the FSDS universe refresh after 14 days. Company facts refresh after 3 days, using the day companyfacts.zip was published, then universe-facts is rebuilt. Stats run when any of those refresh, or when the stats file is missing."]
     [:table
      [:thead
       [:tr [:th "Task"] [:th "As of"] [:th "Limit"] [:th "Status"] [:th "Detail"]]]
      [:tbody
       (for [item items]
         (let [label (state-label item job)]
           [:tr {:key (name (:id item))}
            [:td (:label item)]
            [:td (as-of item)]
            [:td (if-let [n (:max-days item)]
                   (str n " days")
                   "when a dataset refreshes")]
            [:td {:class (case label
                           "running" "ingress-run"
                           "needs download" "ingress-due"
                           "done" "ingress-ok"
                           "ingress-ok")}
             label]
            [:td (:summary item)]]))]]
     (cond
       running? [:button {:type "button" :disabled true}
                 (str "Running "
                      (some (fn [item]
                              (when (= (:id item) (:current job))
                                (:label item)))
                            items)
                      "…")]
       due? [:button {:type "button"
                      :data-on:click (h/action (start!))}
             "Download"]
       :else [:button {:type "button" :disabled true} "Up to date"])
     (when-let [err (:error job)]
       [:p.error err])
     (when (= :done (:status job))
       [:p.ingress-ok "Finished."])
     (when (seq (:lines job))
       [:pre.ingress-log (str/join "\n" (:lines job))])]))

(defn ingress-page
  "Form-2: subscribe to the job atom once, then render from it."
  [req]
  (h/watch! job*)
  (fn [req]
    (page req)))
