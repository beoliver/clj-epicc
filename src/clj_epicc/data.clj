(ns clj-epicc.data
  (:require [clojure.set :as set]))

(defprotocol ISubterms
  (subterms [this] "return a set of terms"))

(defprotocol IGroundSubterms
  (ground-subterms [this] "return a set of terms"))

(defprotocol IVariables
  (variables [this] "return a set of variables"))
(defprotocol INegatable
  (negate [this]))
(defprotocol IEditableMatrix
  (next-clause-index [this])
  (insert-clause [this clause])
  (delete-clause [this index]))
(defprotocol ISubstitution
  (apply-substitution [this substitution]))
(defprotocol ILeanCoP
  (leancop-str [this]))

(defprotocol ILiterals
  (predicates [this])
  (equalities [this]))

(defrecord Variable [label])
(defn variable [label] (->Variable label))
(defn variable? [x] (instance? Variable x))

(defrecord Function [label args])

(defn function
  ([label] (function label []))
  ([label args] (->Function label (vec args))))
(defn function? [x] (instance? Function x))
(defn non-constant-function? [x] (and (instance? Function x) (seq (:args x))))
(defn constant? [x] (and (function? x) (empty? (:args x))))

(defrecord Predicate [label args sign])
(defn predicate
  ([label args] (predicate label args true))
  ([label args sign] (->Predicate label (vec args) sign)))
(defn predicate? [x] (instance? Predicate x))
(defn pos-predicate? [x] (and (predicate? x) (:sign x)))
(defn neg-predicate? [x] (and (predicate? x) (not (:sign x))))

(defrecord Equality  [left right sign])
(defn equality
  ([left right] (equality left right true))
  ([left right sign] (->Equality left right sign)))
(defn equality? [x] (instance? Equality x))
(defn pos-equality? [x] (and (equality? x) (:sign x)))
(defn neg-equality? [x] (and (equality? x) (not (:sign x))))

(defrecord Clause   [index literals])
(defn clause
  [index literals] (->Clause index (set literals)))
(defn clause? [x] (instance? Clause x))

(defrecord Matrix   [problem-id index->clause term->clause pos-eqs neg-eqs predicates])
(defn matrix [id clauses]
  (reduce (fn [m c] (insert-clause m c))
          (map->Matrix {:problem-id id :index->clause {} :term->clause {} :pos-eqs #{} :neg-eqs #{} :predicates {}})
          clauses))
(defn matrix? [x] (instance? Matrix x))

(defn literals [clause] (:literals clause))

(defn update-clause [matrix {:keys [index] :as clause}]
  (-> matrix
      (delete-clause index)
      (insert-clause clause)))

(defn matrix-contains-equality? [{:keys [index->clause] :as matrix}]
  (boolean (some (fn [{:keys [literals] :as clause}]
                   (some equality? literals)) (vals index->clause))))

(extend-protocol INegatable
  Predicate
  (negate [this] (update this :sign not))
  Equality
  (negate [this] (update this :sign not)))

(extend-protocol ILeanCoP
  Variable
  (leancop-str [x] (str (:label x)))
  Function
  (leancop-str [{:keys [label args] :as f}]
    (if (empty? args)
      (str label)
      (->> args
           (map leancop-str)
           (interpose ", ")
           (apply str)
           (format "%s(%s)" label))))
  Predicate
  (leancop-str [{:keys [label args sign] :as p}]
    (let [pred-str (if (empty? args)
                     (str label)
                     (->> args
                          (map leancop-str)
                          (interpose ", ")
                          (apply str)
                          (format "%s(%s)" label)))]
      (if sign
        pred-str
        (format "-(%s)" pred-str))))
  Equality
  (leancop-str [{:keys [left right sign] :as e}]
    (let [eq-str (format "%s = %s" (leancop-str left) (leancop-str right))]
      (if sign
        eq-str
        (format "-(%s)" eq-str))))
  Clause
  (leancop-str [{:keys [literals] :as c}]
    (->> literals
         (map leancop-str)
         (interpose ", ")
         (apply str)
         (format "[%s]")))
  Matrix
  (leancop-str [{:keys [problem-id index->clause] :as m}]
    (let [clauses (sort-by :index (vals index->clause))]
      (->> clauses
           (map leancop-str)
           (interpose ", ")
           (apply str)
           (format "cnf('%s', conjecture, [%s])." problem-id)))))

(extend-protocol ISubterms
  Variable
  (subterms [this] #{this})
  Function
  (subterms [{:keys [args] :as function}] (reduce into #{function} (map subterms args)))
  Predicate
  (subterms [{:keys [args]}] (reduce into #{} (map subterms args)))
  Equality
  (subterms [{:keys [left right]}] (into (subterms left) (subterms right)))
  Clause
  (subterms [{:keys [literals]}] (reduce into #{} (map subterms literals)))
  Matrix
  (subterms [this] (set (keys (:term->clause this)))))

;;; ground subterms... all subterms of a term such that they contain no variables

(defprotocol IGroundSubtermsLowLevel
  (ground-subterms-low-level [this]))

(extend-protocol IGroundSubtermsLowLevel
  Variable
  (ground-subterms-low-level [this] [false #{}])
  Function
  (ground-subterms-low-level [this]
    (let [[is-ground? xs :as result]
          (reduce (fn [[is-ground? xs :as acc] t]
                    (let [[child-is-ground? ys] (ground-subterms-low-level t)]
                      [(and is-ground? child-is-ground?)
                       (set/union xs ys)]))
                  [true #{}]
                  (:args this))
          all-ground-subterms (if is-ground? (conj xs this) xs)]
      [is-ground? all-ground-subterms])))

(defn is-ground-term? [term] (first (ground-subterms-low-level term)))

(extend-protocol IGroundSubterms
  Variable
  (ground-subterms [this] #{})
  Function
  (ground-subterms [this] (second (ground-subterms-low-level this)))
  Predicate
  (ground-subterms [{:keys [args]}]
    (reduce into #{} (map ground-subterms args)))
  Equality
  (ground-subterms [{:keys [left right]}] (into (ground-subterms left)
                                                (ground-subterms right)))
  Clause
  (ground-subterms [{:keys [literals]}] (reduce into #{} (map ground-subterms literals)))
  Matrix
  (ground-subterms [this]
    (set (filter is-ground-term? (keys (:term->clause this))))))

(extend-protocol IVariables
  Variable
  (variables [this] #{this})
  Function
  (variables [{:keys [args]}] (reduce into #{} (map variables args)))
  Predicate
  (variables [{:keys [args]}] (reduce into #{} (map variables args)))
  Equality
  (variables [{:keys [left right]}] (into (variables left) (variables right)))
  Clause
  (variables [{:keys [literals]}] (reduce into #{} (map variables literals)))
  Matrix
  (variables [this]
    (set (filter variable? (keys (:term->clause this))))))

(extend-protocol ILiterals
  Clause
  (predicates [c] (set (filter predicate? (:literals c))))
  (equalities [c] (set (filter equality? (:literals c))))
  Matrix
  (predicates [m] (reduce into #{} (map predicates (vals (:index->clause m)))))
  (equalities [m] (reduce into #{} (map equalities (vals (:index->clause m))))))

(extend-protocol ISubstitution
  Variable
  (apply-substitution [x sigma] (get sigma x x))
  Function
  (apply-substitution [f sigma]
    (if-let [g (get sigma f)]
      g
      (update f :args (fn [xs] (mapv #(apply-substitution % sigma) xs)))))
  Predicate
  (apply-substitution [p sigma]
    (update p :args (fn [xs] (mapv #(apply-substitution % sigma) xs))))
  Equality
  (apply-substitution [e sigma]
    (-> e
        (update :left #(apply-substitution % sigma))
        (update :right #(apply-substitution % sigma))))
  Clause
  (apply-substitution [c sigma]
    (update c :literals (fn [xs] (set (map #(apply-substitution % sigma) xs)))))
  Matrix
  (apply-substitution [m sigma]
    (let [domain (keys sigma)
          candidate-clauses (reduce into (vals (select-keys (:term->clause m) domain)))]
      (assert (every? is-ground-term? domain))
      (reduce (fn [m i]
                (let [c (get-in m [:index->clause i])
                      c' (apply-substitution c sigma)]
                  (if (= c c')
                    m
                    (-> (delete-clause m i)
                        (insert-clause c')))))
              m candidate-clauses))))

(extend-protocol IEditableMatrix
  Matrix
  (next-clause-index [{:keys [index->clause]}]
    (inc (apply max (or (seq (keys index->clause)) [0]))))
  (insert-clause [this {:keys [index literals] :as clause}]
    (let [contains-pos-eqs? (boolean (some pos-equality? literals))
          contains-neg-eqs? (boolean (some neg-equality? literals))
          matrix-with-preds (reduce (fn [matrix {:keys [label sign]}]
                                      (update-in matrix [:predicates label sign] (fnil conj #{}) index))
                                    this (filter predicate? literals))]
      (-> (reduce (fn [matrix t]
                    (update-in matrix [:term->clause t] (fnil conj #{}) index))
                  matrix-with-preds (subterms clause))
          (assoc-in [:index->clause index] clause)
          (update :pos-eqs #(if contains-pos-eqs?
                              (conj (set %) index)
                              %))
          (update :neg-eqs #(if contains-neg-eqs?
                              (conj (set %) index)
                              %)))))
  (delete-clause [this index]
    (if-let [{:keys [literals] :as c} (get-in this [:index->clause index])]
      (let [matrix-with-preds-removed
            (reduce (fn [matrix {:keys [label sign]}]
                      (let [xs (disj (get-in matrix [:predicates label sign]) index)]
                        (if (empty? xs)
                          (update-in matrix [:predicates label] dissoc sign)
                          (assoc-in matrix [:predicates label sign] xs))))
                    this (filter predicate? literals))
            matrix-with-terms-and-preds-removed
            (reduce (fn [matrix t]
                      (let [xs (disj (get-in matrix [:term->clause t]) index)]
                        (if (empty? xs)
                          (update matrix :term->clause dissoc t)
                          (assoc-in matrix [:term->clause t] xs))))
                    matrix-with-preds-removed (subterms c))]
        (-> matrix-with-terms-and-preds-removed
            (update :index->clause dissoc index)
            (update :pos-eqs #(disj (set %) index))
            (update :neg-eqs #(disj (set %) index))
            ))
      this)))

(defn lex-ordering [s t]
  [s t]
  "variable < constant < function"
  (assert (string? (:label s)) [s (type s) (str s)])
  (assert (string? (:label t)) [t (type t) (str t)])
  (let [s-type (cond (constant? s) :constant
                     (variable? s) :variable
                     (function? s) :function)
        t-type (cond (constant? s) :constant
                     (variable? t) :variable
                     (function? t) :function)]
    (case [s-type t-type]
      [:variable :variable] (compare (:label s) (:label t))
      [:constant :constant] (compare (:label s) (:label t))
      [:function :function]
      (let [sn (count (:args s)) tn (count (:args t))]
        (cond (= s t) 0
              (< sn tn) -1
              (> sn tn) 1
              :else (reduce (fn [base-case [a b]]
                              (let [x (lex-ordering a b)]
                                (if-not (= 0 x)
                                  (reduced x)
                                  base-case)))
                            (Long/signum (compare (:label s) (:label t)))
                            (map vector (:args s) (:args t)))))
      [:constant :variable] 1
      [:function :variable] 1
      [:function :constant] 1
      -1)))
