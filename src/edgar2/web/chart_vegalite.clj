(ns edgar2.web.chart-vegalite
  "Client-side Vega-Lite chart. Embeds the `spec` attribute with vega-embed."
  (:require [hyper.core :as h]))

(h/defc chart-vegalite
  "Render a Vega-Lite spec via vega-embed.

  `spec` is a Vega-Lite spec object (Hyper JSON-serializes the map to the
  attribute and the client parses it back). A canvas view is embedded into a
  host div. On spec change the previous view is finalized and a new one is
  embedded; on unmount the view is finalized."
  {:require [["https://cdn.jsdelivr.net/npm/vega-embed@6/+esm" :as vegaEmbed]]}
  [{:keys [spec]}]
  (render
    [:div.chart-vegalite {:style {:width "100%" :display "block"}}])
  (mount [root]
    (letfn [(embed! []
              (let [host (.querySelector root ".chart-vegalite")
                    cur (.-spec ctx)
                    prev (.-result ctx)
                    gen (inc (or (.-gen ctx) 0))]
                (set! (.-gen ctx) gen)
                (when prev
                  ((.-finalize prev))
                  (set! (.-result ctx) nil))
                (when (and host cur)
                  (set! (.-innerHTML host) "")
                  (-> (vegaEmbed/default host cur
                                         {:actions false
                                          :mode "vega-lite"
                                          :renderer "canvas"})
                      (.then (fn [res]
                               (if (= gen (.-gen ctx))
                                 (set! (.-result ctx) res)
                                 ((.-finalize res)))))
                      (.catch (fn [err] (js/console.error err)))))))]
      (set! (.-spec ctx) spec)
      (set! (.-embed ctx) embed!)
      (embed!)))
  (update [_root _old]
    (set! (.-spec ctx) spec)
    (when (.-embed ctx) ((.-embed ctx))))
  (unmount [_root]
    (when-let [res (.-result ctx)]
      ((.-finalize res)))
    (set! (.-result ctx) nil)))
