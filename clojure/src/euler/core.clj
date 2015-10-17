(ns euler.core
  (:require
    [euler.util :as util])
  (:gen-class))

(def euler1 (apply + (filter #(or (util/divides % 5) (util/divides % 3)) (range 1 1000))))

(def euler2 (apply + (filter even? (take-while #(< % 4000000) util/fib-seq))))

(def euler3 (last (util/factors 600851475143)))

(def euler4
  (let [palindromes (filter util/is-palindrome? (range (* 999 999) 1 -1))]
    (first (filter #(not-empty (util/divisors-in-range % (util/upper-sqrt %) 999)) palindromes))))

(def euler5
  (let [fac-map (util/least-factor-map 1 20)
        prime-products (map #(int (Math/pow (first %) (last %))) fac-map)]
    (apply * prime-products)))

(def euler6
  (let [r (range 1 (inc 100))]
    (- (int (Math/pow (apply + r) 2))
       (apply + (map #(int (Math/pow % 2)) r)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Euler 1:" euler1)
  (println "Euler 2:" euler2)
  (println "Euler 3:" euler3)
  (println "Euler 4:" euler4)
  (println "Euler 5:" euler5)
  (println "Euler 6:" euler6))
