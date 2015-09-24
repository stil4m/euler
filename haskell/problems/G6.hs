module Problems.G6 where

import Data.List
import UTIL
import Data.Char
import Data.Ratio
import Control.Arrow
import ONeillPrimes

--52. Permuted multiples
euler52 :: Integer
euler52 = head $ head $ filter arePermutations $ map euler52multiples [1..]

arePermutations :: [Integer] -> Bool
arePermutations x = foldl (\b a -> b && head ss == a) True (tail ss) where
  ss = map (sort.show) x

euler52multiples x = map (*x) [1..6]

-- 53. Combinatoric selections
euler53 :: Int
euler53 = length $ filter (>1000000) [ combinatorics n r | n <- [1..100], r <- [0..n] ]


-- 55.
lychrel :: Integer -> Bool
lychrel x = lychrel' 50 (sumReverse x) where
  -- lychrel' :: Integer-> Integer -> Bool
  lychrel' 0 _ = False
  lychrel' n x | isPalindrome x = True
               | otherwise = lychrel' (n-1) (sumReverse x)
  sumReverse x = x + (read (reverse (show x)) :: Integer)

-- 56. Powerful digit sum
euler56 :: Int
euler56 = maximum [sum (map digitToInt (show (x^y))) | x <- [1..100], y <- [1..100]]

-- 57. Square root convergents
euler57 :: Int
euler57 = length $ filter(\(p,q) -> length p /= length q) $ map (show *** show)  $ expand 1000 (3/2)

expand :: Integral a => Integer -> Ratio a -> [(a, a)]
expand 0 x = []
expand n x = (numerator x, denominator x) : expand (n-1) (1 + 1/(1 + x))

-- 60. Prime pair sets
distributePrimes :: [Integer] -> [[Integer]] -> [[Integer]]
distributePrimes [] res = res
distributePrimes (p:ps) res = distributePrimes ps (map (p:) (filter (isPrimePairWith p) ([]:res)) ++ res)

isPrimePairWith :: Integer -> [Integer] -> Bool
isPrimePairWith n = foldl (\b a -> b && primePair n a) True

primePair :: Integer -> Integer -> Bool
primePair n m = prime a && prime b where
  a = read (show n ++ show m) :: Integer
  b = read (show m ++ show n) :: Integer
