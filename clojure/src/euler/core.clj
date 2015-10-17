(ns euler.core
  (:require
    [euler.util :as util])
  (:gen-class))

(def euler1 (apply + (filter #(or (= 0 (mod % 5)) (= 0 (mod % 3))) (range 1 1000))))

(def euler2 (apply + (filter even? (take-while #(< % 4000000) util/fib-seq))))

(def euler3 (last (util/factors 600851475143)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Euler 1:" euler1)
  (println "Euler 2:" euler2)
  (println "Euler 3:" euler3))
