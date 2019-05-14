(ns clj-epicc.leancop
  (:require [clojure.java.shell :as sh]
            [clj-epicc.data :as data])
  (:import [java.io
            File
            FileWriter
            BufferedWriter]))

(defmulti get-env identity)

(defmethod get-env :default
  [x] (System/getenv x))

(defn write-matrix! [out-path matrix]
  (with-open [writer
              (-> out-path
                  ^FileWriter FileWriter.
                  ^BufferedWriter BufferedWriter.)]
    (.append writer (data/leancop-str matrix))))

(defn- call-leancop
  [path timeout]
  (let [prolog (get-env "PROLOG_PATH")
        leancop (get-env "LEANCOP_PATH")]
    (sh/sh leancop path "--prolog" prolog "-q" "--timeout" (str timeout))))

(defn check-path
  "run leancop for max `timeout-seconds` on file located at `path`.
   only outputs `:theorem`, `:non-theorem` or `:timeout`"
  [path timeout-seconds]
  (let [start-ms (System/currentTimeMillis)
        {:keys [out err] :as x} (call-leancop path timeout-seconds)
        stop-ms (System/currentTimeMillis)
        millis (float (/ (- stop-ms start-ms) 1000))]
    (merge {:time (min timeout-seconds millis)}
           (cond (re-find #"is a Theorem" out) {:result :theorem}
                 (re-find #"TIMEOUT" out) {:result :timeout}
                 :else {:result :non-theorem}))))

(defn check
  [{:keys [matrix-id] :as matrix} timeout-seconds]
  (if-not matrix
    {:result :error :time timeout-seconds}
    (let [tmp ^File (java.io.File/createTempFile matrix-id ".txt")
          path (.getAbsolutePath tmp)]
      (write-matrix! path matrix)
      (let [result (check-path path timeout-seconds)]
        (.delete tmp)
        result))))
