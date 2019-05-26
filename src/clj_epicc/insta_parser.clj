(ns clj-epicc.insta-parser
  (:require [clj-epicc.data  :as data]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))

;;; functions for building up a matrix using the `epicc.data` records.

(defn parse-term [var-fn [op label & args :as term]]
  (case op
    :variable (data/variable (var-fn label))
    :constant (data/function label [])
    :function (data/function label (mapv (partial parse-term var-fn) args))))

(defn parse-literal [var-fn [op & args :as literal]]
  (case op
    :not       (data/negate (parse-literal var-fn (first args)))
    :predicate (data/predicate (first args) (mapv (partial parse-term var-fn) (rest args)))
    :equality  (data/equality (parse-term var-fn (first args)) (parse-term var-fn (second args)))))

(defn parse-clause [index [op & literals :as clause]]
  (let [vars (atom {})
        var-index (atom 1)
        var-fn (fn var-fn [label]
                 (if-let [x (get @vars label)]
                   x
                   (let [x-index @var-index
                         x (format "X_%s_%s" index x-index)]
                     (swap! var-index inc)
                     (swap! vars assoc label x)
                     x)))]
    (data/clause index (set (map (partial parse-literal var-fn) literals)))))

(defn parse-matrix [[op id & clauses :as matrix]]
  (data/matrix (second id) (set (map parse-clause (map inc (range)) clauses))))

(defn parse-with-grammar [grammar s]
  (let [parser (insta/parser grammar :auto-whitespace :standard)]
    (parse-matrix (insta/parse parser s :trace false))))

(defn parse [path]
  (-> "leancopClause.grammar"
      io/resource
      (parse-with-grammar (slurp path))))
