(ns edgar2.web.controls
  "Shared Hyper form controls."
  (:require [hyper.core :as h]))

(defn options
  "Option maps with string `:value` and `:label` for `choices`."
  [choices]
  (mapv (fn [choice] {:value (str choice) :label (str choice)}) choices))

(h/defc param-select
  "Labelled select. Emits `param-select-change` with `:param` and the
  chosen string `:value`."
  [{:keys [label param value options disabled]}]
  (event ::select-changed [e]
    (emit "param-select-change"
          {:param param
           :value (.. e -target -value)}))
  (render
    (let [current (if (nil? value) "" (str value))]
      [:label.screen-controls (str (or label "") " ")
       [:select {:disabled disabled :on {:change ::select-changed}}
        (for [option options]
          (let [v (or (.-value option) "")
                l (or (.-label option) v)]
            [:option {:key v :value v :selected (= v current)}
             l]))]])))
