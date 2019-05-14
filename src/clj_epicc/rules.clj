(ns clj-epicc.rules
  (:require [clj-epicc.data :as data]
            [clj-epicc.mgu :as mgu]
            [clojure.set :as set]))

(defprotocol Rule
  (applicable? [rule clause])
  (explain [this example]))

(defrecord ReductionResult
    [noop clause matrix explanation valid delete invalid destructive])

(defprotocol LocalRule
  (candidate-clause-indexes [rule matrix]
    "Should return a list of clause indexes that
     are possible candidates for the rule.
     As some rules rely on global information - even though
     they only update a clause on a local level...
     This is to avoid having to run the rule on every clause
     if global information can be used to narrow down the
     list of possible clauses that the rule can be used on")
  (apply-local-rule [rule matrix clause]
    "where clause has an index that was returned from
     candidate-clause-indexes.
     The clausse index should not be changed.
     A local rule return a possibly new clause.
     If no clause is returned then it is removed from the matrix.
     result is wrapped in a `LocalReductionResult` record"))

(defprotocol GlobalRule
  (apply-global-rule [rule matrix]
    "should update the matrix in a safe manner
     and return the result in a `ReductionResult` record"))

(def noop
  (map->ReductionResult {:noop true}))
(defn valid [clause explanation]
  (map->ReductionResult {:clause clause :explanation explanation :valid true}))
(defn redundant [clause explanation]
  (map->ReductionResult {:clause clause :explanation explanation :delete true}))
(defn contradictory [clause explanation]
  (map->ReductionResult {:clause clause :explanation explanation :delete true}))

(defn destructive [matrix explanation]
  (map->ReductionResult {:matrix matrix :explanation explanation :destructive true}))


;;; .......................................................................................
;;; HELPERS ...............................................................................
;;; .......................................................................................

(defn indexes-of-clauses-by [literals-pred matrix]
  (seq (reduce-kv (fn [indexes i clause]
                    (if (literals-pred (data/literals clause))
                      (conj indexes i)
                      indexes)) [] (:index->clause matrix))))

;;; .......................................................................................
;;; SATISFIABLE clauses ...................................................................
;;; .......................................................................................

(defrecord Empty_Clause []
  LocalRule
  (candidate-clause-indexes [rule matrix]
    (keys (:index->clause matrix)))
  (apply-local-rule [rule matrix {:keys [literals] :as clause}]
    (if (empty? literals)
      (valid clause "found an empty clause")
      noop)))

(defrecord Positive_Equality_MGU []
  LocalRule
  (candidate-clause-indexes [rule matrix]
    (reduce-kv (fn [indexes index {:keys [literals] :as clause}]
                 (if (every? data/pos-equality? literals)
                   (conj indexes index)
                   indexes))
               []
               (select-keys (:index->clause matrix) (:pos-eqs matrix))))
  (apply-local-rule [rule matrix {:keys [literals] :as clause}]
    (if (apply mgu/mgu literals)
      (valid clause (format "found a mgu for clause containing only positive equality %s" (data/leancop-str clause)))
      (if-let [some-clause-contains-negation?
               (indexes-of-clauses-by (partial some data/neg-equality?) matrix)]
        noop
        (redundant clause "no mgu for clause but no negated equality in matrix")))))

;;; .......................................................................................
;;; CONTRADICTORY clauses .................................................................
;;; .......................................................................................

(defrecord Contradictory_Predicates []
  LocalRule
  (candidate-clause-indexes [rule matrix]
    (indexes-of-clauses-by (fn [literals]
                             (and (some data/pos-predicate? literals)
                                  (some data/neg-predicate? literals))) matrix))
  (apply-local-rule [rule matrix {:keys [literals] :as clause}]
    (let [pos-preds (filter data/pos-predicate? literals)
          neg-preds (filter data/neg-predicate? literals)
          shared-labels (set/intersection
                         (set (map :label pos-preds))
                         (set (map :label neg-preds)))]
      (if-not (seq shared-labels)
        noop
        ;; clause contains both pos and neg predicates with the same label
        (let [pos-eqs (filter data/pos-equality? literals)
              t-closure (mgu/transitive-closure
                         (into {} (for [{:keys [left right]} (filter data/pos-equality? literals)]
                                    [left right])))
              closures (set (vals t-closure))
              sigma (reduce (fn [sigma closure]
                              (let [minimal-term (first (sort data/lex-ordering closure))
                                    theta (zipmap closure (repeat minimal-term))]
                                (merge sigma theta))) {} closures)
              pos-pred-rw (set (map #(dissoc (data/apply-substitution % sigma) :sign) pos-preds))
              neg-pred-rw (set (map #(dissoc (data/apply-substitution % sigma) :sign) neg-preds))]
          (if (seq (set/intersection pos-pred-rw neg-pred-rw))
            (contradictory clause (format "equality implies contradictory predicates in %s" (data/leancop-str clause)))
            noop))))))

(defrecord Contradictory_Negations []
  LocalRule
  (candidate-clause-indexes [rule matrix] (:neg-eqs matrix))
  (apply-local-rule [rule matrix {:keys [literals] :as clause}]
    (let [pos-eqs (filter data/pos-equality? literals)
          t-closure (mgu/transitive-closure
                     (into {} (for [{:keys [left right]} (filter data/pos-equality? literals)]
                                [left right])))
          closures (set (vals t-closure))
          sigma (reduce (fn [sigma closure]
                          (let [minimal-term (first (sort data/lex-ordering closure))
                                theta (zipmap closure (repeat minimal-term))]
                            (merge sigma theta))) {} closures)
          neg-eqs (map #(data/apply-substitution % sigma) (filter data/neg-equality? literals))]
      (if (some #(= (:left %) (:right %)) neg-eqs)
        (contradictory clause (format "equality implies contradictory negated equality in %s" (data/leancop-str clause)))
        noop))))

;;; .......................................................................................
;;; REDUNDANT clauses .....................................................................
;;; .......................................................................................

(defrecord Isloated_Prediacates []
  LocalRule
  (candidate-clause-indexes [rule matrix]
    (reduce-kv (fn [indexes label tables]
                 (let [pos (get tables true #{})
                       neg (get tables false #{})]
                   (cond (empty? pos) (set/union indexes neg)
                         (empty? neg) (set/union indexes pos)
                         :else        indexes)))
               #{} (:predicates matrix)))
  (apply-local-rule [rule matrix {:keys [literals] :as clause}]
    (redundant clause "clause can never be closed")))

;;; .......................................................................................
;;; REWRITE clauses .......................................................................
;;; .......................................................................................
(defn glob-rew [matrix]
  (let [neg-unit-clauses
        (sort-by :index (filter (fn [{:keys [literals] :as c}] (= 1 (count literals)))
                                (vals
                                 (select-keys (:index->clause matrix) (:neg-eqs matrix)))))]
    (let [[m updates] (reduce (fn [[matrix history] {:keys [index literals] :as c}]
                                (let [{:keys [left right] :as e} (first literals)]
                                  ;; only ever rewrite ground terms!
                                  (if (seq (data/variables e))
                                    [matrix history]
                                    (let [sub-left  (set (data/subterms left))
                                          sub-right (set (data/subterms right))
                                          touched   (fn [s] (disj (get-in matrix [:term->clause s] #{}) index))
                                          explain   (fn [s t]
                                                      (let [touched-str (apply str (interpose "," (touched s)))]
                                                        (format "removed clause %s at index %s and rewrote %s with %s in clauses %s"
                                                                (data/leancop-str c)
                                                                index (data/leancop-str s) (data/leancop-str t) touched-str)))
                                          new-matrix (data/delete-clause matrix index)]
                                      (cond (and (empty? (touched left))
                                                 (empty? (touched right))) [matrix history]
                                            (and (contains? sub-right left)
                                                 (empty? (touched right))) [matrix history]
                                            (contains? sub-right left)  [(data/apply-substitution new-matrix {right left})
                                                                         (conj history (explain right left))]
                                            (and (contains? sub-left right)
                                                 (empty? (touched left)))  [matrix history]
                                            (contains? sub-left  right) [(data/apply-substitution new-matrix {left right})
                                                                         (conj history (explain left right))]
                                            (empty? (touched left))     [(data/apply-substitution new-matrix {right left})
                                                                         (conj history (explain right left))]
                                            (empty? (touched right))    [(data/apply-substitution new-matrix {left right})
                                                                         (conj history (explain left right))]
                                            :else                       [(data/apply-substitution new-matrix {left right})
                                                                         (conj history (explain left right))])))))
                              [matrix []]
                              neg-unit-clauses)]
      (if (empty? updates)
        noop
        (destructive m updates)))))

(defn rewrite-left-right [matrix]
  (let [neg-unit-clauses
        (sort-by :index (filter (fn [{:keys [literals] :as c}] (= 1 (count literals)))
                                (vals
                                 (select-keys (:index->clause matrix) (:neg-eqs matrix)))))]
    (let [[m updates] (reduce (fn [[matrix history] {:keys [index literals] :as c}]
                                (let [{:keys [left right] :as e} (first literals)]
                                  ;; only ever rewrite ground terms!
                                  (if (seq (data/variables e))
                                    [matrix history]
                                    (let [sub-left  (set (data/subterms left))
                                          sub-right (set (data/subterms right))
                                          touched   (fn [s] (disj (get-in matrix [:term->clause s] #{}) index))
                                          explain   (fn [s t]
                                                      (let [touched-str (apply str (interpose "," (touched s)))]
                                                        (format "removed clause %s at index %s and rewrote %s with %s in clauses %s"
                                                                (data/leancop-str c)
                                                                index (data/leancop-str s) (data/leancop-str t) touched-str)))
                                          new-matrix (data/delete-clause matrix index)]
                                      [(data/apply-substitution new-matrix {left right})
                                       (conj history (explain left right))]
                                      ))))
                              [matrix []]
                              neg-unit-clauses)]
      (if (empty? updates)
        noop
        (destructive m updates)))))

(defrecord Unit_Rewrite_Rule []
  GlobalRule
  (apply-global-rule [rule matrix] (glob-rew matrix)))

(defrecord Unit_Rewrite_Rule_Left_Right []
  GlobalRule
  (apply-global-rule [rule matrix] (rewrite-left-right matrix)))

;;; ------------------------------------------------------------------------------------------
;;; Running The Rules ------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------

(defn terminate-at [timeout]
  (+ (System/currentTimeMillis) timeout))

(defn terminate? [terminates-at]
  (> (System/currentTimeMillis)
     terminates-at))

(defn run-local-reduction-rule
  [{:keys [matrix history terminates-at] :as state} rule]
  (if-let [candidates (seq (candidate-clause-indexes rule matrix))]
    (reduce (fn [state clause-index]
              (if (terminate? terminates-at)
                (reduced (update state :history assoc :timeout true))
                (let [current-matrix (:matrix state)
                      pre-clause (get-in current-matrix [:index->clause clause-index])
                      {:keys [noop clause explanation valid delete destructive]}
                      (apply-local-rule rule current-matrix pre-clause)
                      new-state
                      (cond noop   state
                            delete (-> state
                                       (update-in [:history :logs] conj {:rule (.getSimpleName (class rule))
                                                                         :clause clause-index
                                                                         :explanation explanation})
                                       (update :history assoc :valid valid)
                                       (update :matrix data/delete-clause clause-index))
                            :else  (-> state
                                       (update-in [:history :logs] conj {:rule (.getSimpleName (class rule))
                                                                         :clause clause-index
                                                                         :explanation explanation})
                                       (update :history assoc :valid valid)
                                       (update :matrix data/update-clause clause)))]
                  (assert (data/matrix? (:matrix new-state)))
                  (if valid
                    (reduced new-state)
                    new-state))))
            state
            candidates)
    state))

(defn run-global-reduction-rule
  [{:keys [matrix history terminates-at] :as state} rule]
  (let [{:keys [noop matrix explanation valid invalid destructive]} (apply-global-rule rule
                                                                                       (with-meta matrix
                                                                                         {:terminates-at terminates-at}))]
    (if noop
      state
      (-> state
          (assoc :matrix matrix)
          (update-in [:history :logs] conj {:rule (.getSimpleName (class rule)) :explanation explanation})
          (update :history assoc :valid valid)
          (update :history #(if (:destructive %) % (assoc % :destructive destructive)))
          (update :history assoc :invalid invalid)))))


(defn run-reduction-rule [{:keys [terminates-at matrix history] :as state} rule]
  (cond (terminate? terminates-at)                  (update state :history assoc :timeout true)
        (:valid history)                            state
        (:invalid history)                          state
        (satisfies? LocalRule rule)  (run-local-reduction-rule state rule)
        (satisfies? GlobalRule rule) (run-global-reduction-rule state rule)))


(defn run-reductions
  ([matrix rules] (run-reductions matrix rules {:timeout-ms 1000 :max-iterations 100}))
  ([matrix rules {:keys [timeout-ms max-iterations] :as opts}]
   (let [terminates-at (terminate-at timeout-ms)]
     (loop [previous-state {:terminates-at terminates-at :matrix matrix :history {:logs [] :iteration 0}}]
       (cond (terminate? terminates-at)                     (update previous-state :history assoc :timeout true)
             (= max-iterations (:iteration previous-state)) (update previous-state :history assoc :max-iterations true)
             :else
             (let [{:keys [matrix history] :as state} (reduce run-reduction-rule previous-state rules)]
               (cond (:timeout history)                  state
                     (:valid   history)                  state
                     (:invalid history)                  state
                     (= matrix (:matrix previous-state)) state
                     :else                               (recur (update-in state [:history :iteration] inc)))))))))



(defn reduce-matrix
  "returns a posibly updated matrix with metadata that logs the actions performed"
  ([matrix rules] (reduce-matrix matrix rules 10000))
  ([matrix rules timeout-ms]
   (if-not (data/matrix-contains-equality? matrix)
     matrix
     (let [result (run-reductions matrix rules
                                  {:timeout-ms timeout-ms :max-iterations 100})]
       (with-meta (:matrix result) (select-keys result [:history]))))))


(def safe-rules
  [(->Empty_Clause)
   (->Positive_Equality_MGU)
   (->Contradictory_Predicates)
   (->Contradictory_Negations)
   (->Isloated_Prediacates)])

(def unsafe-rules
  (conj safe-rules (->Unit_Rewrite_Rule)))

(def unsafe-left-right-rewrite
  (conj safe-rules (->Unit_Rewrite_Rule_Left_Right)))


(defn unsafe-reduce-matrix
  [matrix timeout]
  (reduce-matrix matrix unsafe-rules timeout))

(defn only-unsafe-reduce-matrix
  [matrix timeout]
  (reduce-matrix matrix [(->Unit_Rewrite_Rule)] timeout))

(defn unsafe-reduce-matrix-left-right
  [matrix timeout]
  (reduce-matrix matrix unsafe-left-right-rewrite timeout))

(defn safe-reduce-matrix
  [matrix timeout]
  (reduce-matrix matrix safe-rules timeout))
