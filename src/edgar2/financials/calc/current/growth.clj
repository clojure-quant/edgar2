(ns edgar2.financials.calc.current.growth
  (:require
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [tablecloth.api :as tc]
   [edgar2.financials.calc.util :as u]))

(defn- avg
  [xs]
  (when (seq xs)
    (/ (double (reduce + xs)) (count xs))))

(defn most-recents [ds n]
  (tc/select-rows ds (range n)))

(defn average-over [ds concept n]
  (u/assert-cols ds [concept])
  (let [col (-> ds
                (most-recents n)
                (get concept))]
    (->> col 
         (remove nil?)  
         avg)))

(defn prior-col [col]
  (let [c (count col)
        col (into [] col)
        end (dec c)]
    ;(println "col: " col)
    (map
     (fn [idx]
       (if (< idx end)
         (nth col (inc idx))
         nil))
     (range c))))

(defn map-cur-prior [ds col f]
  (let [prior (prior-col (get ds col))
        ds-with-col (tc/add-column ds :prior prior)
        ds-compareable (most-recents ds-with-col (dec (tc/row-count ds-with-col)))
        p (get ds-compareable :prior)
        c (get ds-compareable col)]
    (map f p c)))

(defn- yoy-pct
  [prev cur]
  (when (and (number? prev) (number? cur) (not (zero? (double prev))))
    (* 100.0 (/ (- (double cur) (double prev)) (double prev)))))

(defn increased? [prev cur]
  ;(println "prev: " prev " cur: " cur)
  (when (and (number? prev) (number? cur))
    (if (>= (double cur) (double prev)) 1 0)))

(defn direction-score
  "Share of years that did not fall versus the prior year, from 0 to 100."
  [ds col n]
  (u/assert-cols ds [col])
  (let [ds-y (most-recents ds n)
        steps (map-cur-prior ds-y col increased?)
        steps (remove nil? steps)]
    (when (seq steps)
      (* 100.0 (/ (double (reduce + steps)) (count steps))))))

(defn growth
  [ds n col]
  (let [ds-y (most-recents ds n)
        prct (map-cur-prior ds-y col yoy-pct)]
    ;(get prct 0)
    prct))

(defn growth-most-recent [ds col n]
  (-> (growth ds n col)
      (first)))

(defn calc-growth [concept-math-ds]
  (let [c (tc/row-count concept-math-ds)
        n (cond
            (>= c 7) 7
            (>= c 5) c
            :else
            false)]
    ;(println "calc growth for years: " n)
    (if n
      {:avg-profit-margin (average-over concept-math-ds :net-income-margin n)
       :sales-growth-avg-yoy (growth-most-recent concept-math-ds :revenue n)
       :revenue-score (direction-score concept-math-ds :revenue n)
       :operating-income-score (direction-score concept-math-ds :net-income n)}
      {})))



(comment

  (def d (tc/dataset [{:a 6.0}
                      {:a 5.0}
                      {:a nil}
                      {:a 4.0}
                      {:a 3.0}
                      {:a 2.0}
                      {:a 1.0}
                      {:a 1.0}
                      {:a 1.0}
                      {:a 1.0}]))
  d

  (direction-score d 7 :a)
  (growth d 7 :a)
  (growth-most-recent d 7 :a)
  (prior-col (:a d))
  (map-cur-prior d :a (fn [p c] (println "prior: " p "cur: " c)))
  (most-recents d 4)

 ; 
  )