(ns clj-epicc.core
  (:require
   [clj-epicc.insta-parser :as p]
   [clj-epicc.rules :as r]
   [clj-epicc.axioms :as ax]
   [clj-epicc.modification-method :as mm]
   [clj-epicc.leancop :as leancop])
  (:gen-class))

(defn run-reductions! [input-file output-file reduction-fn]
  (let [matrix (p/parse input-file)]
    (println "Matrix parsed")
    (if (not (or (seq (:pos-eqs matrix)) (seq (:neg-eqs matrix))))
      (println "problem does not contain equality")
      (let [red (reduction-fn matrix)]
        (leancop/write-matrix! output-file red)
        (println (format "file written to %s" output-file))))))

(def reduction-lookup
  {:axioms (fn [m] (ax/add-axioms m))
   :red    (fn [m] (ax/add-axioms (r/unsafe-reduce-matrix m 1000)))
   :red-lr (fn [m] (ax/add-axioms (r/unsafe-reduce-matrix-left-right m 1000)))
   :mm     (fn [m] (mm/modification-method m))})

(defn -main
  [in-file out-file reduction-name & args]
  (let [reduction-kw (keyword reduction-name)
        reduction-function
        (if-not (reduction-lookup reduction-kw)
          (do (println (format "no reduction rule set called '%s'. Switching to `:axioms`" reduction-kw))
              (get reduction-lookup :axioms))
          (do (println (format "using reduction rule set '%s'" reduction-kw))
              (get reduction-lookup reduction-kw)))]
    (run-reductions! in-file out-file reduction-function)
    (System/exit 0)))
