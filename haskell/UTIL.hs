-- Getting Started
module UTIL

where

import Data.Char
import ONeillPrimes (primes)
import Data.List
import Data.List.Split

ldp :: Integer -> Integer
ldp = ldpf primes

ldpf :: [Integer] -> Integer -> Integer
ldpf [] _     = 0
ldpf (p:ps) n | rem n p == 0 = p
              | (p^2) > n    = n
              | otherwise    = ldpf ps n

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ldp n

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n

mxmInt :: [Int] -> Int
mxmInt []     = error "empty list"
mxmInt [x]    = x
mxmInt (x:xs) = max x (mxmInt xs)

run :: Integer -> [Integer]
run n | n < 1  = error "argument not positive"
      | n == 1 = [1]
      | otherwise = n : run (next n)
      where
      next :: Integer -> Integer
      next n = if even n then div n 2 else 3 * n + 1

factorial :: Integer -> Integer
factorial n = product [1..n]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

toBin :: Integer -> Integer
toBin 0 = 0
toBin n = foldr (\a b -> b * 10 + a) 0 (helper n)
    where
    helper 0 = []
    helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
             | n `mod` 2 == 0 = 0 : helper (n `div` 2)


isSquare :: Integer -> Bool
isSquare x = (root * root) == x where
    root ::  Integer
    root = toInteger $ floor $ sqrt $ fromInteger x

combinatorics :: Integer -> Integer -> Integer
combinatorics n r = div (factorial n) (factorial r * factorial (n - r))

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = r . while p f


digitSum :: Integer -> Integer
digitSum n = toInteger $ sum $ map digitToInt (show n)

isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)

wordToSum :: String -> Int
wordToSum = foldl (\b a -> b + (ord (toLower a) - 96)) 0

divides :: Integral a => a -> a -> Bool
divides m n = mod m n == 0

toPentagon :: Integer -> Integer
toPentagon x =  div (x * (3 * x - 1)) 2

toTriangular :: Integer -> Integer
toTriangular x = div (x * (x + 1)) 2

divisors :: Integer -> [Integer]
divisors n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n


numDivisors :: Integer -> Integer
numDivisors n = toInteger $ product (map ((+1).length) $ group $ factors n) - 1

-- Path Sum Pyramide
pyramidToIntegers :: String -> [[Integer]]
pyramidToIntegers s = map (map (\p -> read p :: Integer) . words) (lines s)

foldRows :: [Integer] -> [Integer] -> [Integer]
foldRows (a:b:xs) (y:z:zs) = y + max a b : foldRows (b:xs) (z:zs)
foldRows [x] (y:_) = [y+x]
foldRows _ y = y

foldPyramid :: [[Integer]] -> [Integer]
foldPyramid [] = error "Invalid input"
foldPyramid [x] = x
foldPyramid (x:y:ys) = foldPyramid (foldRows (0:x) y : ys)

computeMaxPath :: String -> Integer
computeMaxPath s = maximum $ foldPyramid (pyramidToIntegers s)

commaSepToStringList :: String -> [String]
commaSepToStringList s = map (tail.init) (splitOn "," s)











--
