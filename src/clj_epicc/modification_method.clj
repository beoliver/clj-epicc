(ns clj-epicc.modification-method
  (:require [clj-epicc.data   :as data]
            [clj-epicc.axioms :as axioms]
            [clojure.set  :as set]))

(declare rewrite-predicate rewrite-equality rewrite-function)

(defn e-modification [formulae]
  (loop [[f & formulae] formulae
         new-forms #{}]
    (if (nil? f)
      new-forms
      (let [[p companions] (cond (data/predicate? f) (rewrite-predicate f)
                                 (data/equality? f)  (rewrite-equality  f))]
        (recur (set/union formulae companions) (conj new-forms p))))))

(defn rewrite-predicate
  "returns [ predicate #{companions} ]"
  [predicate]
  (let [[args companions]
        (reduce (fn [[new-args companions] arg]
                  (if (data/variable? arg)
                    [(conj new-args arg) companions]
                    (let [v (data/variable (str (gensym "X_")))
                          e (data/equality arg v true)]
                      [(conj new-args v) (conj companions e)])))
                [[] #{}] (:args predicate))]
    [(assoc predicate :args args) companions]))

(defn rewrite-function
  [function]
  (let [[args companions]
        (reduce (fn [[new-args companions] arg]
                  (if (data/variable? arg)
                    [(conj new-args arg) companions]
                    (let [v (data/variable (str (gensym "X_")))
                          e (data/equality arg v true)]
                      [(conj new-args v) (conj companions e)])))
                [[] #{}] (:args function))]
    [(assoc function :args args) companions]))

(defn rewrite-equality
  "returns [ equality #{companions} ]"
  [{:keys [left right] :as equality}]
  (let [[left' l-companions]  (if (data/function? left)  (rewrite-function left) [left #{}])
        [right' r-companions] (if (data/function? right) (rewrite-function right) [right #{}])]
    [(-> equality
         (assoc :left left')
         (assoc :right right'))
     (set/union l-companions r-companions)]))


(defn all-paths [choices]
  (let [xs (map vector (first choices))]
    (loop [paths (map vector (first choices))
           [xs & remaining] (rest choices)]
      (if (nil? xs)
        paths
        (let [paths (reduce (fn [acc choice]
                              (into acc
                                    (map #(conj % choice) paths)))
                            []
                            xs)]
          (recur paths remaining))))))

(defn s-modification
  "returns a set of sets (each set is a clause)"
  [formulae]
  (let [m (group-by data/neg-equality? formulae)
        neg-eqs (get m true)
        others  (get m false)
        choices (map vector
                     neg-eqs
                     (for [{:keys [left right] :as e} neg-eqs]
                       (data/equality right left false)))
        paths (all-paths choices)]
    (if (empty? neg-eqs)
      #{formulae}
      (set (for [p paths]
             (set (into p others)))))))

(defn t-modification
  [formulae]
  (reduce (fn [new-forms form]
            (if-not (data/neg-equality? form)
              (conj new-forms form)
              (let [w (data/variable (str (gensym "X_")))
                    s (:left form)
                    t (:right form)
                    eq-1 (data/equality t w true)
                    eq-2 (data/equality s w false)]
                (conj new-forms eq-1 eq-2))))
          #{} formulae))

(declare ste-modification)

(defn ste-modification [formulae]
  (->> formulae
       e-modification
       s-modification
       (map t-modification)))

(defn modification-method [{:keys [problem-id] :as matrix}]
  (let [clauses (vals (:index->clause matrix))
        m (data/matrix problem-id [])
        generated-clauses (->> clauses
                               (map (comp ste-modification data/literals))
                               (reduce into)
                               (map (fn [index xs] (data/clause index xs)) (range)))]
    (->> generated-clauses
         (data/matrix problem-id)
         axioms/add-reflexivity)))
