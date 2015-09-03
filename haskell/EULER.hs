module EULER

where
import UTIL
import INPUT
import Data.String
import Data.Char
import GHC.Float
import Data.List
import ONeillPrimes

-- 1. Multiples of 3 and 5
euler1 :: Integer
euler1 = sum $ filter (\x -> rem x 3  == 0 || rem x 5 == 0) [1..999]

-- 2. Even Fibonacci numbers

fib x y max | x + y >= max =  []
            | otherwise = (x + y) : fib y (x + y) max
fibonaci = fib 0 1
euler2 :: Integer
euler2 = sum (filter even (fibonaci 4000000))

-- 3. Largest prime factor
euler3 :: Integer
euler3 = last (factors 600851475143)

-- 4. Largest palindrome product
isPalindrome :: Int -> Bool
isPalindrome x = (show x) == reverse (show x)
euler4 :: Integer
euler4 = toInteger $ maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

-- 5. Smallest multiple
smallestMultiple :: Integer -> [Integer]
smallestMultiple x
    | x == 0 = []
    | (rem (product sub) x) == 0 = 1 : sub
    | otherwise = (ldp x) : sub
    where sub = (smallestMultiple (x- 1))
euler5 :: Integer
euler5 = product (smallestMultiple 20)

-- 6. Sum square difference
sumSquareDiff x = (sum [1..x])^2 - (sum (map (^2) [1..x]))
euler6 :: Integer
euler6 = sumSquareDiff 100

-- 7. 10001st prime
euler7 :: Integer
euler7 = last (take 10001 primes)

-- 8. Largest product in a series
longestStreak :: Int -> [Int] -> [Int]
longestStreak _ [] = []
longestStreak n xs = (product (take n xs)) : longestStreak n (tail xs)
euler8 :: Integer
euler8 = toInteger $ maximum (longestStreak 13 (map digitToInt euler8input) )

-- 9. Special Pythagorean triplet
euler9 :: Integer
euler9 = toInteger $ product [float2Int a, float2Int b,float2Int  c]
    where (a, b, c) = head [(a, b, sqrt(a^2 + b^2) ) | a <- [1..500], b <- [1..500], a < b, (a + b + sqrt(a^2 + b^2) == 1000) ]

-- 10. Summation of primes
euler10 :: Integer
euler10 = sum (takeWhile (<2000000) primes)

-- 11. Largest product in a grid

-- 12. Highly divisible triangular number

-- 13. Large sum
euler13 :: Integer
euler13 = read (take 10 $ show $ sum euler13input) :: Integer

 -- 14. Longest Collatz sequence
collatz_sequence = run
collatz_sequences :: [Integer] -> [[Integer]]
collatz_sequences x = (map collatz_sequence x)
max_collatz_sequence = mxmInt (map length (collatz_sequences [1..999999]))
euler14 :: Integer
euler14 = toInteger max_collatz_sequence

-- 15. Lattice paths
--https://wiki.haskell.org/Blow_your_mind
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]
euler15 = head $ drop 20 $ last (take 41 pascal)

-- 32. Pandigital products
isPandigital :: (Integer, Integer, Integer) -> Bool
isPandigital (x, y,z) = sort (show x ++ show y ++ show z) == "123456789"

pandigitalTriples = filter isPandigital [(x,y,z) | x <- [1..1987], y <- [1..(x-1)], let z = x * y ]

euler32 = sum $ rmdups [ z | (_,_,z) <- pandigitalTriples]

-- 49. Prime permutations
fourDigitPrimes :: [Integer]
fourDigitPrimes = dropWhile (<1000) $ takeWhile (<(10000- 2*3330)) primes

fourDigitPrimeTriplets :: [(Integer,Integer,Integer)]
fourDigitPrimeTriplets = filter (\(x, y, z) -> prime y && prime z) $ map (\x -> (x, x+3330, x + 6660)) fourDigitPrimes

integerPermutation :: Integer -> Integer -> Bool
integerPermutation x y = (sort $ show x) == (sort $ show y)
findPrimePermutationTriplets = filter(\(x, y, z) -> integerPermutation x  y && integerPermutation x z) fourDigitPrimeTriplets

euler49 :: Integer
euler49 = last $ map (\(x, y, z) -> x * 100000000 + y * 10000 + z) findPrimePermutationTriplets



eulerAll :: [(Integer, Integer)]
eulerAll = [(1, euler1), (2, euler2), (3, euler3)
            , (3, euler3)
            , (4, euler4)
            , (5, euler5)
            , (6, euler6)
            , (7, euler7)
            , (8, euler8)
            , (9, euler9)
            , (10, euler10)
            , (13, euler13)
--            , (14, euler14) long running
            , (15, euler15)
            , (32, euler32)
            , (49, euler49)
            ]

