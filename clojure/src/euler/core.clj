(ns euler.core
  (:require
    [euler.util :as util]
    [euler.input :as input]
    [euler.primes :as primes])
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

(def euler7
  (last (take 10001 (primes/lazy-primes))))

(def euler8
  (apply max (map #(apply * %) (util/parts-of 13 input/input8))))

(def euler9
  (apply * (flatten (for
                      [x (range 1 333)]
                      (for
                        [y (range (inc x) 500)
                         :let [z (- 1000 y x)]
                         :when (> z y)
                         :when (= (+ (Math/pow x 2) (Math/pow y 2)) (Math/pow z 2))]
                        [x y z])))))

(def euler10
  (apply + (take-while #(< % 200000) (primes/lazy-primes))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Euler 1:" euler1)
  (println "Euler 2:" euler2)
  (println "Euler 3:" euler3)
  (println "Euler 4:" euler4)
  (println "Euler 5:" euler5)
  (println "Euler 6:" euler6)
  (println "Euler 7:" euler7)
  (println "Euler 8:" euler8)
  (println "Euler 9:" euler9)
  (println "Euler 10:" euler10))
