(ns edgar2.web.ingress-eodhd
  "Status of the EODHD bar and split downloads.

  Download runs whatever is due. Download all always pulls bars and splits."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.eodhd.ingress :as ingress]
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

(defn- start-job!
  "Run `run-fn` on a background thread. Further clicks do nothing until it finishes."
  [run-fn]
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
                      ;; Always publish progress lines; throttle the rest so
                      ;; hyper is not flooded by chatty import noise.
                      (if (or (str/includes? line "downloaded")
                              (> (- (System/currentTimeMillis) @flushed-at) 400))
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
            (run-fn {}))
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

(defn start!
  "Run the due EODHD downloads on a background thread."
  []
  (start-job! ingress/run))

(defn start-all!
  "Download all bar series and splits on a background thread."
  []
  (start-job! ingress/run-all))

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
     [:h1 "Ingress EODHD"]
     [:p "Download refreshes bars and splits for US common and preferred stocks with at least $5 million turnover (latest session close × volume) — about 2,000 names. Download all drops that floor and pulls every US equity EODHD lists (~18,000). Each series is 50 years of daily data; files live in data/eodhd/bardb. The screen reads them through the eodhd split service, so prices are split-adjusted. The token is read from $QUANTASTORE/edgar2.edn."]
     [:table
      [:thead
       [:tr [:th "Task"] [:th "As of"] [:th "Limit"] [:th "Status"] [:th "Detail"]]]
      [:tbody
       (for [item items]
         (let [label (state-label item job)]
           [:tr {:key (name (:id item))}
            [:td (:label item)]
            [:td (or (:date item) "—")]
            [:td (str (:max-days item) " days")]
            [:td {:class (case label
                           "running" "ingress-run"
                           "needs download" "ingress-due"
                           "done" "ingress-ok"
                           "ingress-ok")}
             label]
            [:td (:summary item)]]))]]
     (if running?
       [:button {:type "button" :disabled true}
        (str "Running "
             (some (fn [item]
                     (when (= (:id item) (:current job))
                       (:label item)))
                   items)
             "…")]
       [:div.ingress-actions
        (if due?
          [:button {:type "button"
                    :data-on:click (h/action (start!))}
           "Download"]
          [:button {:type "button" :disabled true} "Up to date"])
        [:button {:type "button"
                  :data-on:click (h/action (start-all!))}
         "Download all"]])
     (when-let [err (:error job)]
       [:p.error err])
     (when (= :done (:status job))
       [:p.ingress-ok "Finished."])
     (when (seq (:lines job))
       [:pre.ingress-log (str/join "\n" (:lines job))])]))

(defn ingress-eodhd-page
  [req]
  (h/watch! job*)
  (fn [req]
    (page req)))
