(ns euler.util)

(def fib-seq
  (lazy-cat [0 1] (map + (rest fib-seq) fib-seq)))

(defn next-factor
  [min max n]
  (let [lower-factor (first (filter #(= 0 (mod n %)) (range min (inc max))))]
    (if lower-factor lower-factor n)))

(defn factors-from
  [from target]
  (let [max (int (Math/floor (Math/sqrt target)))
        next (next-factor from max target)]
    (if (= target next)
      [target]
      (cons next (factors-from next (/ target next))))))

(defn factors
  [n]
  (factors-from 2 n))
