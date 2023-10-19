#!/usr/bin/env lein exec

(ns join-json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- expression [expr]
  (eval (read-string expr)))

(defn- output [data]
  (println (json/write-str data))
  (flush))

(defn- join-inner [map]
  (doseq [entry map]
    (let [[k [values0 values1]] entry]
      (doseq [v0 values0
              v1 values1]
        (output [v0 v1])))))

(defn- join-left [map]
  (doseq [entry map]
    (let [[k [values0 values1]] entry]
      (if (seq values0)
        (doseq [v0 values0
                v1 values1]
          (output [v0 v1]))
        (output [nil (first values1)]))))

(defn- join-right [map]
  (doseq [entry map]
    (let [[k [values0 values1]] entry]
      (if (seq values1)
        (doseq [v0 values0
                v1 values1]
          (output [v0 v1]))
        (output [(first values0) nil]))))

(defn- join-outer [map]
  (doseq [entry map]
    (let [[k [values0 values1]] entry]
      (if (seq values0)
        (doseq [v0 values0
                v1 values1]
          (output [v0 v1]))
        (doseq [v1 values1]
          (output [nil v1])))))

(defn -main [& args]
  (let [i0 (atom -1)
        i1 (atom -1)
        ii (atom 0)
        map (atom {})
        sandbox (assoc {} :i0 i0 :i1 i1)
        key0 (expression (first args))
        key1 (expression (second args))
        left (boolean (get args 2))
        right (boolean (get args 3))
        outer (boolean (get args 4))]

    (doseq [line (line-seq *in* :encoding "UTF-8")]
      (swap! i0 inc)
      (let [d (try
                (json/read-str line)
                (catch Exception e
                  (println (str "stdin:" @i0))
                  (println line)
                  (println "^")
                  (println (str "SyntaxError: " (.getMessage e)))
                  (System/exit 1)))
            k0 (expression (first args) d)
            entry (get @map k0 [() ()])]
        (if left
          (let [v0 (first entry)]
            (reset! map (assoc @map k0 [(conj (first entry) d) (second entry)])))
          (reset! map (assoc @map k0 [(first entry) (conj (second entry) d)])))
        (if (= @ii 0)
          (reset! ii 1)))
      (swap! i1 inc)
      (let [d (try
                (json/read-str line)
                (catch Exception e
                  (println (str "stdin:" @i1))
                  (println line)
                  (println "^")
                  (println (str "SyntaxError: " (.getMessage e)))
                  (System/exit 1)))
            k1 (expression (second args) d)
            entry (get @map k1 [() ()])]
        (if right
          (let [v1 (second entry)]
            (reset! map (assoc @map k1 [(conj (first entry) d) (second entry)])))
          (reset! map (assoc @map k1 [(first entry) (conj (second entry) d)])))
        (if (= @ii 1)
          (reset! ii 2)))
      (if (= @ii 3)
        (if left (join-left @map)
          (if right (join-right @map)
            (join-outer @map)))))))

(-main *command-line-args*)
