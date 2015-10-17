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
  (apply + (take-while #(< % 2000000) (primes/lazy-primes))))

(defn coords-to-vals
  [cords seq]
  (map #(map (fn [c]
               (nth seq
                    (+ (first c) (* 20 (second c)))))
             %)
       cords))

(def euler11
  (let [horizontal (for [x (range 0 20)] (take 20 (drop (* x 20) input/input11)))
        vertical (for [x (range 0 20) :let [y (range 0 20)]] (map #(nth input/input11 (+ x (* % 20))) y))
        diagonal-up (for [x (range 0 39)
                          :let [y (range 0 20)]]
                      (filter #(and (< (first %) 20) (> (first %) -1)) (map #(do [(- x %) %]) y)))
        diagonal-down (map #(map (fn [e] [(- 19 (first e)) (second e)]) %) diagonal-up)
        parts (apply concat (map #(util/parts-of 4 %) (concat horizontal
                                                              vertical
                                                              (coords-to-vals diagonal-down input/input11)
                                                              (coords-to-vals diagonal-up  input/input11))))]
    (apply max (map #(apply * %) parts))))


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
  (println "Euler 10:" euler10)
  (println "Euler 11:" euler11))
