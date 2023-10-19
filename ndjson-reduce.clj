#!/usr/bin/env lein exec

(ns reduce-json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- expression [expr initial]
  (eval (read-string expr) (-> {} (assoc :initial initial))))

(defn- output [data]
  (println (json/write-str data))
  (flush))

(defn -main [& args]
  (let [i (atom -1)
        sandbox (-> (assoc {} :i i) (assoc :d nil) (assoc :p nil))
        unset (or (= (count args) 2)
                   (reset! sandbox (assoc @sandbox :p (expression (second args) nil))))
        reduce (expression (first args) (if unset nil @sandbox))
        context (-> sandbox
                     (dissoc :i)
                     (assoc :d nil)
                     (dissoc :p)
                     (assoc :p nil))]

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
            p (if unset (reset! unset false) (reduce d))]
        (reset! sandbox (assoc @sandbox :d d :p p))))
    (output (get @sandbox :p))))

(-main *command-line-args*)
