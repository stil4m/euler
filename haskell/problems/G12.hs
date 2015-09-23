module Problems.G12 where

import Data.List
import UTIL

-- 119. Digit power sum
euler119 :: Integer
euler119 = dropWhile (<=10) (sort $ map snd $ concatMap factorValues [1..1000]) !! 29
factorValues :: Integer -> [(Integer, Integer)]
factorValues n = filter (\(p,_) -> p == n) $ map ((\q -> (digitSum q, q)) . (n^)) [1..100]

-- 120. Square remainders
euler120 :: Integer
euler120 = sum (map squareRem [3..1000])

squareRem :: Integer -> Integer
squareRem n = if odd n then n * (n-1) else n * (n-2)
