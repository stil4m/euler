module Problems.G2 where

import UTIL
import INPUT
import Data.Char
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List

-- 11. Largest product in a grid
euler11 :: Int
euler11 = maximum $ fourSums euler11gridValue $ concat (horizontal ++ vertical  ++ diagonalDown  ++ diagonalUp)
euler11gridValue :: (Int,Int) -> Int
euler11gridValue (r,c) = (euler11input !! r) !!c

fourSums :: ((Int,Int) -> Int) -> [(Int,Int)] -> [Int]
fourSums g (p:q:r:s:ss) = g p * g q * g r * g s : fourSums g (q : r : s: ss)
fourSums _ _ = []

horizontal,vertical,diagonalDown,diagonalUp :: [[(Int,Int)]]
horizontal = map (\n -> filter (\(r,_) -> r == n) coordinates) [0..19]
vertical = map (\n -> filter (\(_,c) -> c == n) coordinates) [0..19]
diagonalDown = map (\n -> filter (\(r,c) -> r - c == n) coordinates) [-19..19]
diagonalUp = map (\n -> filter (\(r,c) -> c + r == n) coordinates) [0..39]

coordinates :: [(Int,Int)]
coordinates = [(r,c) | r <- [0..19], c <- [0..19]]

-- 12. Highly divisible triangular number
euler12 :: Integer
euler12 = head $ filter (\q -> numDivisors q >= 500) $ map toTriangular [1..]

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

-- 16.
euler16 :: Int
euler16 = sum (map digitToInt (show (2^1000)))


-- 17.


-- 18. Maximum path sum I
euler18 :: IO ()
euler18 = do
       content <- readFile "p018_triangle.txt"
       let linesOfFile = computeMaxPath content
       print linesOfFile

-- 19. Counting Sundays
euler19 :: Int
euler19 = length $ filter (==7) $ map (\(_,_,d) -> d) [toWeekDate $ fromGregorian y m 1| y <- [1901..2000], m <- [1..12]]

-- 20. Factorial digit sum
euler20 :: Int
euler20 = sum (map digitToInt (show (product [1..100])))
