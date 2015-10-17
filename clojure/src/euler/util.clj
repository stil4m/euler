(ns euler.util)

(def fib-seq
  (lazy-cat [0 1] (map + (rest fib-seq) fib-seq)))


(def factors)