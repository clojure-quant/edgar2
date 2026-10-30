(ns edgar2.screen.util
  "Filter pipelines shared by the screens.

  apply-and-pipeline walks {:label :pred} steps in order. Each
  predicate runs on the tickers that survived the previous step.
  The first stage is the incoming count, labeled universe.
  report-pipeline-info formats those stages as the count lines the
  screen prints, label on the left and survivors on the right.")

(defn at-least
  "Predicate: `k` is a number and is at least `minimum`."
  [k minimum]
  (fn [row]
    (let [v (get row k)]
      (and (number? v)
           (>= (double v) (double minimum))))))

(defn between
  "Predicate: `k` is a number and is between `lo` and `hi`, inclusive."
  [k lo hi]
  (fn [row]
    (let [v (get row k)]
      (and (number? v)
           (<= (double lo) (double v) (double hi))))))

(defn apply-and-pipeline
  "Filter `tickers` through `filter-ands` in order.

  Each step is {:label :pred}. :pred is applied to the tickers still
  in the list. Returns {:tickers <survivors> :stages [{:label :n} ...]}.
  The first stage is the incoming count, labeled universe. Each later
  :n is how many tickers remain after that step."
  [tickers filter-ands]
  (let [tickers (vec tickers)]
    (reduce (fn [{:keys [tickers] :as result} {:keys [label pred]}]
              (when-not (ifn? pred)
                (throw (ex-info "Filter step needs a :pred function"
                                {:label label})))
              (let [kept (filterv pred tickers)]
                {:tickers kept
                 :stages (conj (:stages result)
                               {:label (str label) :n (count kept)})}))
            {:tickers tickers
             :stages [{:label "universe" :n (count tickers)}]}
            filter-ands)))

(defn report-pipeline-info
  "Lines for `pipeline-result`: each stage label, and the count still
  in the list. Counts share a column on the right."
  [pipeline-result]
  (let [stages (vec (:stages pipeline-result))
        widest (reduce max 0 (map #(count (:label %)) stages))
        label-width (max 64 (+ widest 2))]
    (mapv (fn [{:keys [label n]}]
            (format (str "%-" label-width "s%6d") label (long n)))
          stages)))
