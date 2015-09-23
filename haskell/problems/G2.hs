module Problems.G2 where

import UTIL
import INPUT

-- 11. Largest product in a grid

-- 12. Highly divisible triangular number

-- 13. Large sum
euler13 :: Integer
euler13 = read (take 10 $ show $ sum euler13input) :: Integer

 -- 14. Longest Collatz sequence
collatzSequence :: Integer -> [Integer]
collatzSequence = run

collatzSequences :: [Integer] -> [[Integer]]
collatzSequences = map collatzSequence

maxCollatzSequence :: Int
maxCollatzSequence = mxmInt (map length (collatzSequences [1..999999]))

euler14 :: Integer
euler14 = toInteger maxCollatzSequence

-- 15. Lattice paths
--https://wiki.haskell.org/Blow_your_mind
pascal :: [[Integer]]
pascal = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]

euler15 :: Integer
euler15 = last (take 41 pascal) !! 20
