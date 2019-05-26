(ns clj-epicc.parser-v2
  (:require [clj-epicc.data :as data])
  (:import [java.io
            Reader
            StringReader
            FileReader
            PushbackReader]
           [java.lang
            StringBuilder]))

;; (declare parse*)

;; (defrecord State [index string])
;; (defn make-state [string] (map->State {:index 0 :string string}))

;; (defn parse-string
;;   ([^String input] (parse-string input nil))
;;   ([^String input timeout-ms]
;;    (let [fmatrix (future (parse* (make-state input)))]
;;      (if-not timeout-ms
;;        (deref fmatrix)
;;        (let [matrix (deref fmatrix timeout-ms nil)]
;;          (or matrix
;;              (try (future-cancel fmatrix)
;;                   (catch Exception e nil)
;;                   (finally nil))))))))

;; (defn parse-path
;;   ([^String path] (parse-path path nil))
;;   ([^String path timeout-ms]
;;    (let [input (slurp path)]
;;      (parse-string input timeout-ms))))

;; (declare parse-problem)

;; (defn drop-while [char-pred {:keys [index string] :as state}]
;;   (loop [n index]
;;     (if (char-pred (nth string n))
;;       (recur (inc n))
;;       (assoc state :index n)
;;       )))

;; (defn inc-index [state] (update state :index inc))

;; (defn consume-whitespace [state]
;;   (drop-while #(= \space %) state))

;; (defn- parse* [{:keys [index string] :as state}]
;;   (let [state1 (inc-index (drop-while #(not= % \') state))
;;         state2 (drop-while #(not= % \') state1)
;;         problem-name (subs string (:index state1) (:index state2))
;;         ;; find the start of the matrix
;;         state3 (drop-while #(not= % \[) state2)]
;;     (parse-problem state3 (data/matrix problem-name []))))

;; (defn char-at-point [{:keys [index string] :as state}]
;;   (nth string index))

;; (defn assert-char-and-inc [c state]
;;   (assert (= c (char-at-point state)) (format "expected char %s" c))
;;   (inc-index state))

;; (declare parse-clause)

;; (defn- parse-problem
;;   [state matrix]
;;   (loop [state (consume-whitespace (assert-char-and-inc \[ state))
;;          matrix matrix
;;          clause-index 1]
;;     (case (char-at-point state)
;;       \]     matrix
;;       \,     (recur (consume-whitespace (inc-index state)) matrix clause-index)
;;       \[     (let [[state2 clause] (parse-clause state clause-index)
;;                    updated-matrix  (data/insert-clause matrix clause)]
;;                (recur (consume-whitespace state2)
;;                       updated-matrix
;;                       (inc clause-index)))
;;       (throw (new Exception (format "got character %s when parsing matrix" (char-at-point state)))))))


;; (declare parse-formula)

;; (defn- parse-clause
;;   ;; must return [new-state clause]
;;   [state clause-index]
;;   (let [var-index (atom 1)
;;         var-map   (atom {})]
;;     (loop [state (consume-whitespace (assert-char-and-inc \[ state))
;;            formulae (transient #{})]
;;       (case (char-at-point state)
;;         \]     [(inc-index state) (data/clause clause-index (persistent! formulae))] ; consume the whole clause.
;;         \,     (recur (consume-whitespace (inc-index state)) formulae)
;;         (let [[state2 formula] (parse-formula state clause-index var-index var-map)]
;;           (recur (consume-whitespace state2) (conj! formulae formula)))))))

;; (declare parse-term)

;; (defn consume-whitespace [^PushbackReader reader]
;;   (loop []
;;     (let [c ^char (char (.read reader))]
;;       (if (= c \space)
;;         (recur)
;;         (.unread reader (unchecked-int c))))))

;; (defn head-matches [^PushbackReader reader c]
;;   (let [x ^char (char (.read reader))]
;;     (.unread reader (unchecked-int x))
;;     (= c x)))

;; (defn- parse-formula
;;   ;; must return [new-state formula]
;;   ([state clause-index var-index var-map]
;;    (let [state (consume-whitespace state)]
;;      (let [c (char-at-point state)]
;;        (if (= c \-) ;; start of negative predicate
;;          (let [state (consume-whitespace (inc-index state))
;;                c (char-at-point state)]
;;            (if (= c \()
;;              (let [state (consume-whitespace (inc-index state))
;;                    [state2 result] (parse-formula state clause-index var-index var-map false)]
;;                [(assert-char-and-inc \) state2) result])
;;              ;; negation but no parens!!!
;;              (let [[state2 result] (parse-formula (consume-whitespace state) clause-index var-index var-map false)]
;;                [state2 result])))
;;          ;; no negation
;;          (let [[state2 result] (parse-formula (consume-whitespace state) clause-index var-index var-map true)]
;;            [state2 result])))))
;;   ([^PushbackReader state clause-index var-index var-map positive?]
;;    (let [[state2 left] (parse-term reader clause-index var-index var-map)]
;;      (consume-whitespace reader)
;;      (let [c ^char (char (.read reader))
;;            is-predicate? (case c
;;                            \)     true
;;                            \]     true
;;                            \,     true
;;                            \=     false)]
;;        (if is-predicate?
;;          (let [label (:label left)
;;                args  (:args left)
;;                predicate (data/predicate label (vec args) positive?)]
;;            (.unread reader (unchecked-int c))
;;            predicate)
;;          ;; parse the rhs of an equality
;;          (do (consume-whitespace reader)
;;              (let [right (parse-term reader clause-index var-index var-map)]
;;                (data/equality left right positive?))))))))

;; (declare parse-variable parse-function-or-constant)

;; (defn- parse-term
;;   [^PushbackReader reader clause-index var-index var-map]
;;   (let [c ^char (char (.read reader))]
;;     (.unread reader (unchecked-int c))
;;     (if (or (= c \_) (Character/isUpperCase c))
;;       ;; start of a variable
;;       (parse-variable reader clause-index var-index var-map)
;;       (parse-function-or-constant reader clause-index var-index var-map))))

;; (defn- parse-variable
;;   [^PushbackReader reader clause-index var-index var-map]
;;   (let [builder ^StringBuilder (StringBuilder. 16)]
;;     (loop [c ^char (char (.read reader))]
;;       (case c
;;         \,     (.unread reader (unchecked-int c))
;;         \space (.unread reader (unchecked-int c))
;;         \)     (.unread reader (unchecked-int c))
;;         \]     (.unread reader (unchecked-int c))
;;         (do (.append builder c)
;;             (recur ^char (char (.read reader))))))
;;     (let [label (.toString builder)]
;;       (if-let [mapped-var (get @var-map label)]
;;         mapped-var
;;         (let [new-var (data/variable (format "X_%s_%s" clause-index @var-index))]
;;           (swap! var-index inc)
;;           (swap! var-map assoc label new-var)
;;           new-var)))))

;; (defn- parse-function-or-constant
;;   [^PushbackReader reader clause-index var-index var-map]
;;   (let [builder ^StringBuilder (StringBuilder. 16)]
;;     (loop [c ^char (char (.read reader))]
;;       (let [is-constant? (case c
;;                            \)     true
;;                            \]     true
;;                            \,     true
;;                            \space true
;;                            false)]
;;         (cond is-constant? (let [constant (data/function (.toString builder))]
;;                              (.unread reader (unchecked-int c))
;;                              constant)
;;               (= c \()
;;               (let [label (.toString builder)]
;;                 (loop [args (transient [])]
;;                   (let [args (conj! args (parse-term reader clause-index var-index var-map))
;;                         c ^char (.read reader)]
;;                     (cond
;;                       (= c \)) (data/function label (persistent! args))
;;                       (= c \,) (do (.read reader) ;; \space
;;                                    (recur args))))))

;;               :else (do (.append builder c)
;;                         (recur ^char (char (.read reader)))))))))
