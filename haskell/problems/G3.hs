module Problems.G3
where

import Data.List
import UTIL
import ONeillPrimes

-- 21.
euler21 = sum $ map fst [ (x,y) |  (x,y) <- amicablePairs, x /=  y && y < 10001, let z = amicablePairs !! (fromInteger y), snd z == x] where
  amicablePairs = map (\q -> (q, sum $ divisors q)) [0..10000]

-- 26. Reciprocal cycles
euler26 :: Integer
euler26 = snd $ head $reverse $ sort $ map (\q -> (cycleLength (numberDecimals q), q)) (takeWhile (<1000) primes)

numberDecimals :: Integer -> [Integer]
numberDecimals m = drop (length (factors m)) $ numbers' (start m) m where
  start x = head $ dropWhile (<x) $ map (10^) [1..]
  numbers' n x | mod n x == 0 = [div n x]
               | otherwise = div n x : numbers' ((n - x * div n x) * 10) x

cycleLength :: [Integer] -> Int
cycleLength = cycleLength' [] where
  cycleLength' :: [Integer] -> [Integer] -> Int
  cycleLength' _ [] = 0
  cycleLength' [] (y:ys) = cycleLength' [y] ys
  cycleLength' xs ys = if xs == take (length xs) ys then length xs else cycleLength' (xs ++ [head ys]) (tail ys)

-- 27. Quadratic primes

euler27 :: Integer
euler27 = uncurry (*) (snd $ maximum $ map (\q -> (longestPrimeSequence q, q)) [(a, b) | a <- quadraticPrimes, b <- quadraticPrimes])

longestPrimeSequence :: (Integer,Integer) -> Integer
longestPrimeSequence (x,y) = longestPrimeSequence' 1 x y where
  longestPrimeSequence' n a b = if value > 0 && prime value then longestPrimeSequence' (n+1) a b else n where
    value = n^2 + (n*a) + b

quadraticPrimes :: [Integer]
quadraticPrimes = lowPrimes ++ map (*(-1)) lowPrimes where
  lowPrimes = takeWhile (<1000) primes
