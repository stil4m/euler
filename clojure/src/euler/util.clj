(ns euler.util
  (:require
    [euler.primes :as primes]))

(defn divides
  [n m]
  (= 0 (mod n m)))

(defn divides-prop
  [n]
  (partial divides n))

(def fib-seq
  (lazy-cat [0 1] (map + (rest fib-seq) fib-seq)))

(defn next-factor
  [min max n]
  (if-let [lower-factor (first (filter (divides-prop n) (range min (inc max))))]
    lower-factor
    n))

(defn upper-sqrt
  [n]
  (int (Math/ceil (Math/sqrt n))))

(defn lower-sqrt
  [n]
  (int (Math/floor (Math/sqrt n))))

(defn factors-from
  [from target]
  (let [max (lower-sqrt target)
        next (next-factor from max target)]
    (if (= target next)
      [target]
      (cons next (factors-from next (/ target next))))))

(defn factors
  [n]
  (factors-from 2 n))

(defn factors-map
  [n]
  (let [facs (factors n)
        parts (partition-by identity facs)]
    (reduce (fn [b a] (assoc b (first a) (count a))) {} parts)))

(defn divisors-in-range
  [n min max]
  (filter (divides-prop n) (range min (inc max))))

(defn is-palindrome?
  [n]
  (let [str-n (str n)]
    (= (clojure.string/reverse str-n) str-n)))


(defn least-factor-map
  [n m]
  (reduce (fn [b a] (merge-with max (factors-map a) b)) {} (range n (inc m))))

(defn parts-of
  "Divides a sequence in lists with length `n`"
  [n seq]
  (if (< (count seq) n)
    []
    (cons (take n seq) (parts-of n (drop 1 seq)))))