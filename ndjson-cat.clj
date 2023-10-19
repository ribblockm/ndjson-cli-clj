#!/usr/bin/env lein exec

(ns concat-json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell])
  (:gen-class))

(defn cat [input]
  (let [data (atom [])
        stream (if (= input "-")
                 *in*
                 (io/input-stream input))]
    (doseq [line (line-seq (io/reader stream))]
      (swap! data str line))
    (try
      (let [d (json/read-str @data)]
        (output d))
      (catch Exception e
        (println (str (if (= input "-") "stdin" input) ": SyntaxError: " (.getMessage e))
        (System/exit 1)))))

(defn -main [& args]
  (if (empty? args)
    (do
      (output nil)
      (System/exit 0))
    (dorun (map cat args))))

(defn output [data]
  (println (json/write-str data))
  (flush))

(defn -main [& args]
  (if (empty? args)
    (do
      (output nil)
      (System/exit 0))
    (dorun (map cat args)))

(defn -main [& args]
  (if (empty? args)
    (do
      (output nil)
      (System/exit 0))
    (dorun (map cat args)))

(-main *command-line-args*)
