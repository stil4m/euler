module Problems.G1 where

import UTIL
import ONeillPrimes
import Data.Char
import INPUT

-- 1. Multiples of 3 and 5
euler1 :: Integer
euler1 = sum $ filter (\x -> rem x 3  == 0 || rem x 5 == 0) [1..999]

-- 2. Even Fibonacci numbers

fib :: Integer -> Integer -> Integer -> [Integer]
fib x y m | x + y >= m =  []
            | otherwise = (x + y) : fib y (x + y) m

fibonaci :: Integer -> [Integer]
fibonaci = fib 0 1

euler2 :: Integer
euler2 = sum (filter even (fibonaci 4000000))

-- 3. Largest prime factor
euler3 :: Integer
euler3 = last (factors 600851475143)

-- 4. Largest palindrome product
euler4 :: Integer
euler4 = maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

-- 5. Smallest multiple
smallestMultiple :: Integer -> [Integer]
smallestMultiple x
    | x == 0 = []
    | rem (product sub) x == 0 = 1 : sub
    | otherwise = ldp x : sub
    where sub = smallestMultiple (x- 1)
euler5 :: Integer
euler5 = product (smallestMultiple 20)

-- 6. Sum square difference
sumSquareDiff :: Integer -> Integer
sumSquareDiff x = sum [1..x]^2 - sum (map (^2) [1..x])
euler6 :: Integer
euler6 = sumSquareDiff 100

-- 7. 10001st prime
euler7 :: Integer
euler7 = last (take 10001 primes)

-- 8. Largest product in a series
longestStreak :: Int -> [Int] -> [Int]
longestStreak _ [] = []
longestStreak n xs = product (take n xs) : longestStreak n (tail xs)
euler8 :: Integer
euler8 = toInteger $ maximum (longestStreak 13 (map digitToInt euler8input) )

-- 9. Special Pythagorean triplet
euler9 :: Integer
euler9 = product [a, b, c]
    where (a, b, c) = head [(x, y, z) | x <- [1..333], y <- [x..500], let z = 1000 - x - y, x^2 + y^2 == z^2 ]

-- 10. Summation of primes
euler10 :: Integer
euler10 = sum (takeWhile (<2000000) primes)
