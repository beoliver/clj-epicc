(ns clj-epicc.axioms
  (:require [clj-epicc.data :as fol]))

;;; axioms ----------------------------------------------------------------------
;;; -----------------------------------------------------------------------------

(defn- unique-vars [clause-index prefix]
  (map (fn [i] (fol/variable (format "%s_%s_%s" prefix clause-index i))) (range)))

(defn- axioms-of-reflexivity [clause-index]
  (let [[x] (take 1 (unique-vars clause-index "X"))]
    #{(fol/equality x x false)}))

(defn- axioms-of-symmetry [clause-index]
  (let [[x y] (take 2 (unique-vars clause-index "X"))]
    #{(fol/equality x y true)
      (fol/equality y x false)}))

(defn- axioms-of-transitivity [clause-index]
  (let [[x y z] (take 3 (unique-vars clause-index "X"))]
    #{(fol/equality x y true)
      (fol/equality y z true)
      (fol/equality x z false)}))

(defn add-reflexivity [matrix]
  (let [index (fol/next-clause-index matrix)]
    (->> (axioms-of-reflexivity index)
         (fol/clause index)
         (fol/insert-clause matrix))))

(defn add-symmetry [matrix]
  (let [index (fol/next-clause-index matrix)]
    (->> (axioms-of-symmetry index)
         (fol/clause index)
         (fol/insert-clause matrix))))

(defn add-transitivity [matrix]
  (let [index (fol/next-clause-index matrix)]
    (->> (axioms-of-transitivity index)
         (fol/clause index)
         (fol/insert-clause matrix))))

(defn- function-axioms [clause-index label arity]
  (when (pos? arity)
    (let [lvars (take arity (unique-vars clause-index "X"))
          rvars (take arity (unique-vars clause-index "Y"))]
      (into #{(fol/equality (fol/function label lvars) (fol/function label rvars) false)}
            (map #(fol/equality %1 %2 true) lvars rvars)))))

(defn- predicate-axioms [clause-index label arity]
  (let [lvars (take arity (unique-vars clause-index "X"))
        rvars (take arity (unique-vars clause-index "Y"))]
    (when (pos? arity)
      (into #{(fol/predicate label lvars true) (fol/predicate label rvars false)}
            (map #(fol/equality %1 %2 true) lvars rvars)))))

(defn- add-function-axioms [matrix]
  (let [labels-with-arity (->> (fol/subterms matrix)
                               (filter fol/function?)
                               (map (fn [f] {:label (:label f) :arity (count (:args f))}))
                               set)]
    (reduce (fn [matrix {:keys [label arity]}]
              (let [i (fol/next-clause-index matrix)]
                (if-let [axiom-set (function-axioms i label arity)]
                  (->> (fol/clause i axiom-set)
                       (fol/insert-clause matrix))
                  matrix)))
            matrix labels-with-arity)))

(defn- add-predicate-axioms [matrix]
  (let [labels-with-arity (->> (fol/predicates matrix)
                               (map (fn [p] {:label (:label p) :arity (count (:args p))}))
                               set)]
    (reduce (fn [matrix {:keys [label arity]}]
              (let [i (fol/next-clause-index matrix)]
                (if-let [axiom-set (predicate-axioms i label arity)]
                  (->> (fol/clause i axiom-set)
                       (fol/insert-clause matrix))
                  matrix)))
            matrix labels-with-arity)))

(defn add-axioms [matrix]
  (if (empty? (fol/equalities matrix))
    matrix
    (-> matrix
        add-reflexivity
        add-symmetry
        add-transitivity
        add-function-axioms
        add-predicate-axioms)))
