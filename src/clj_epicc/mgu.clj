(ns clj-epicc.mgu
  (:require [clojure.set :as set]
            [clj-epicc.data :as data]))

(defn martelli-montanari-mgu
  "Based on Alberto Martelli and Ugo Montanari algorithm-1"
  [& pairs]
  (let [condition-a (fn [[s t]] (and (not (data/variable? s)) (data/variable? t)))
        condition-b (fn [[s t]] (and (= s t) (data/variable? s) (data/variable? t)))
        condition-c (fn [[s t]] (and (not (data/variable? s)) (not (data/variable? t))))
        condition-d (fn [[s t]] (and (data/variable? s) (not (data/variable? t))))]
    (loop [previous-unifier (set pairs)]
      (let [result (loop [seen nil
                          [[s t :as pair] & unseen] previous-unifier]
                     (cond (nil? pair) seen
                           (condition-a pair) (recur seen (cons [t s] unseen))
                           (condition-b pair) (recur seen unseen)
                           (condition-c pair) (let [s-args (:args s)
                                                    t-args (:args t)]
                                                (cond (not= (:label s) (:label t)) ::error
                                                      (not= (count s-args) (count t-args)) ::error
                                                      :else (->> (map vector s-args t-args)
                                                                 (into unseen)
                                                                 (recur seen))))
                           (condition-d pair) (if (contains? (set (data/variables t)) s)
                                                ::error
                                                (let [sigma {s t}
                                                      sigma-fn (fn [[a b]]
                                                                 [(data/apply-substitution a sigma)
                                                                  (data/apply-substitution b sigma)])]
                                                  (recur (conj (map sigma-fn seen) pair) (map sigma-fn unseen))))
                           :else              (recur (conj seen pair) unseen)))]
        (when-not (= result ::error)
          (let [unifier (set result)]
            (if (= unifier previous-unifier)
              (into {} unifier)
              (recur unifier))))))))

(defn mgu [& equalities]
  (->> equalities
       (map (fn [e] [(:left e) (:right e)]))
       (apply martelli-montanari-mgu)))

;;; generate random unification sets

(defn piles [n coll]
  (->> coll
       vec
       (reduce-kv (fn [m k v]
                    (update m (rem k n) conj v))
                  (zipmap (range n) (repeat [])))
       vals))

(defn transitive-closure [m]
  (let [reflexive-symmetric-closure
        (reduce-kv (fn [closure k v]
                     (-> closure
                         (assoc k (set [k v]))
                         (assoc v (set [k v])))) {} m)]
    (loop [transitive-closure {}
           [k & ks] (keys reflexive-symmetric-closure)]
      (if-not k
        transitive-closure
        (let [k-closure
              (loop [closure #{k}
                     [t & pending] (disj (get reflexive-symmetric-closure k) k)]
                (if-not t
                  closure
                  (let [closure (conj closure t)
                        t-set (set/difference
                               (get reflexive-symmetric-closure t)
                               closure)]
                    (recur closure (set/union (set pending) t-set)))))
              transitive-closure
              (reduce (fn [transitive-closure k]
                        (assoc transitive-closure k k-closure))
                      transitive-closure
                      k-closure)]
          (recur transitive-closure
                 (set/difference (set ks) k-closure)))))))


(defn rewrite-in [transitive-closure term]
  (let [non-reflexive-options (vec (disj (get transitive-closure term #{}) term))]
    (cond (data/variable? term)
          (if-not (empty? non-reflexive-options)
            (let [term' (rand-nth non-reflexive-options)]
              (if (data/non-constant-function? term')
                (update term' :args #(mapv (partial rewrite-in transitive-closure) %))
                term'))
            term)
          (data/function? term)
          (update term :args #(mapv (partial rewrite-in transitive-closure) %)))))

(defn generate-function [label arity transitive-closure]
  (->> transitive-closure
       keys
       shuffle
       (take arity)
       (mapv (partial rewrite-in transitive-closure))
       (data/function label)))

(defn generate-unifiable-equalities [n-eqs n-vars n-constants n-functions]
  (let [vars            (take n-vars (map #(data/variable (str "X_" %)) (range)))
        constants       (take n-constants (map #(data/function (str "c_" %) []) (range)))
        function-labels (take n-functions (map #(str "f_" %) (range)))
        ;; if f1, f2, ... then f1 may occur as an argument to f2
        [constant-vars function-vars free-vars :as data] (piles 3 vars)
        equalities (merge (zipmap constant-vars (shuffle constants))
                          (zipmap free-vars (shuffle (into constant-vars free-vars))))]
    ;; do not include the function vars in the initial closure
    ;; any value selected from a set in the t-closure is derivable
    ;; from the var-assignments
    (loop [t-closure  (transitive-closure equalities)
           [label & f-labels] (shuffle function-labels)
           function-vars function-vars]
      (if-not label
        ;;
        (let [interesting-t-closure
              (reduce-kv (fn [t-closure k closure]
                           (if (= 1 (count closure))
                             t-closure
                             (assoc t-closure k closure)))
                         {} t-closure)
              terms (shuffle (keys interesting-t-closure))]
          (take n-eqs
                (map (fn [term]
                       (let [s (rewrite-in interesting-t-closure term)
                             t (rewrite-in interesting-t-closure term)]
                         (assert (or (data/variable? s) (data/function? s)))
                         (assert (or (data/variable? t) (data/function? t)))
                         (data/equality s t)
                         ))
                     terms)))
        (let [f (generate-function label (inc (rand-int 5)) t-closure)]
          (assert (data/function? f))
          (if (empty? function-vars)
            (let [t-closure (-> t-closure (assoc f #{f}))]
              (recur t-closure f-labels nil))
            (let [x (first function-vars)
                  xs (rest function-vars)
                  t-closure (-> t-closure
                                (assoc f #{f x})
                                (assoc x #{f x}))]
              (recur t-closure f-labels xs))))))))
