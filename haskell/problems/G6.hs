module Problems.G6 where

import Data.List
import UTIL
import Data.Char

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

-- 56. Powerful digit sum
euler56 :: Int
euler56 = maximum [sum (map digitToInt (show (x^y))) | x <- [1..100], y <- [1..100]]
