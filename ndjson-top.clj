#!/usr/bin/env lein exec

(ns select-top-json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- expression [expr]
  (eval (read-string expr)))

(defn- output [data]
  (println (json/write-str data))
  (flush))

(defn- heap-up [heap object i]
  (loop [i i]
    (if (zero? i)
      heap
      (let [j (quot (- i 1) 2)
            parent (nth (:array heap) j)]
        (if (>= (heap-compare heap object parent) 0)
          heap
          (let [array (assoc-in (:array heap) [i] parent
                          [j] object)]
            (recur j (assoc heap :array array))))))))

(defn- heap-down [heap object i]
  (loop [i i]
    (let [r (+ (* 2 i) 2)
          l (- r 1)
          j i
          array (:array heap)
          child (nth array j)]
      (if (and (< l (:n heap)) (< (heap-compare heap (nth array l) child) 0))
        (let [array (assoc-in array [i] (nth array l)
                        [l] child)]
          (recur l (assoc heap :array array)))
        (if (and (< r (:n heap)) (< (heap-compare heap (nth array r) child) 0))
          (let [array (assoc-in array [i] (nth array r)
                          [r] child)]
            (recur r (assoc heap :array array)))
          heap)))))

(defn- heap-compare [heap a b]
  (let [sandbox (:sandbox heap)
        a-expr (:a-expr sandbox)
        b-expr (:b-expr sandbox)
        a-expr-ctx (assoc-in (dissoc sandbox :a-expr :b-expr) [:d] a)
        b-expr-ctx (assoc-in (dissoc sandbox :a-expr :b-expr) [:d] b)]
    (compare (eval a-expr-ctx (read-string a-expr))
             (eval b-expr-ctx (read-string b-expr)))))

(defn- create-heap [k a-expr b-expr]
  {:n 0
   :k k
   :array []
   :compare heap-compare
   :sandbox {:a nil
             :b nil
             :a-expr a-expr
             :b-expr b-expr}})

(defn- push-heap [heap object]
  (let [n (inc (:n heap))]
    (if (>= n (:k heap))
      (pop-heap (assoc heap :n n) object)
      (let [array (conj (:array heap) object)]
        (heap-up (assoc heap :n n :array array) object n)))))

(defn- pop-heap [heap object]
  (if (zero? (:n heap))
    {:done true :value nil}
    (let [n (dec (:n heap))
          removed (first (:array heap))
          object (if (pos? n) (nth (:array heap) n) nil)
          array (conj (assoc (:array heap) 0 object) removed)
          new-heap (assoc heap :n n :array array)]
      (loop [i 0]
        (if (>= (* 2 i) n)
          {:done false :value removed}
          (let [r (+ (* 2 i) 2)
                l (- r 1)
                j i
                array (:array new-heap)
                child (nth array j)]
            (if (and (< l n) (< (heap-compare new-heap (nth array l) child) 0))
              (let [array (assoc-in array [i] (nth array l)
                              [l] child)]
                (recur l (assoc new-heap :array array)))
              (if (and (< r n) (< (heap-compare new-heap (nth array r) child) 0))
                (let [array (assoc-in array [i] (nth array r)
                                [r] child)]
                  (recur r (assoc new-heap :array array)))
                {:done false :value removed})))))))))

(defn -main [& args]
  (let [i (atom -1)
        n (-> args first Integer. int)
        a-expr (-> args second (or "a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN"))
        heap (create-heap n a-expr "a")
        b-expr "b"
        context (-> (dissoc (:sandbox heap) :a-expr :b-expr) (assoc :b nil))]

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
            result (push-heap heap d)]
        (reset! heap (update-in @heap [:sandbox :a] (constantly d))))
    
    (let [result (pop-heap heap)]
      (while (not (:done result))
        (output (:value result))
        (reset! result (pop-heap heap)))))))

(-main *command-line-args*)
