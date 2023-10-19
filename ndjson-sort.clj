#!/usr/bin/env lein exec

(ns sort-json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- expression [expr]
  (eval (read-string expr)))

(defn- output [data]
  (println (json/write-str data))
  (flush))

(defn -main [& args]
  (let [i (atom -1)
        results (atom [])
        sandbox (-> (assoc {} :a nil) (assoc :b nil))
        compare (expression (first args))
        context (-> sandbox
                     (dissoc :a)
                     (assoc :a nil)
                     (dissoc :b)
                     (assoc :b nil))]

    (doseq [line (line-seq *in* :encoding "UTF-8")]
      (swap! i inc)
      (let [d (try
                (json/read-str line)
                (catch Exception e
                  (println (str "stdin:" @i))
                  (println line)
                  (println "^")
                  (println (str "SyntaxError: " (.getMessage e)))
                  (System/exit 1)))
        (swap! results conj d)))
    
    (let [sorted-results (sort-by (fn [a b]
                                    (reset! sandbox (assoc @sandbox :a a :b b))
                                    (compare)))]

      (doseq [r sorted-results]
        (output r))))

(-main *command-line-args*)
