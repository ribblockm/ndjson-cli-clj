#!/usr/bin/env lein exec

(ns transform-json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- expression [expr]
  (eval (read-string expr)))

(defn- requires [module]
  (require (symbol module)))

(defn- output [data]
  (println (json/write-str data))
  (flush))

(defn- run-transform [expression]
  (let [i (atom -1)]
    (doseq [line (line-seq *in* :encoding "UTF-8")]
      (swap! i inc)
      (let [sandbox (assoc {} :i @i)
            d (try
                (json/read-str line)
                (catch Exception e
                  (println (str "stdin:" @i))
                  (println line)
                  (println "^")
                  (println (str "SyntaxError: " (.getMessage e)))
                  (System/exit 1)))
            map (expression d)]
        (if (not (instance? clojure.lang.IPersistentCollection map))
          (do
            (println (str "stdin:" @i))
            (println (if (nil? map) "null" (json/write-str map)))
            (println "^")
            (println "TypeError: not array")
            (System/exit 1))
          (doseq [row map]
            (output row)))))))

(defn -main [& args]
  (if (empty? args)
    (do
      (output nil)
      (System/exit 0))
    (run-transform (expression (first args)))))

(-main *command-line-args*)

