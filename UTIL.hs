-- Getting Started
module UTIL

where

ldp :: Integer -> Integer
ldp n = ldpf primes n

ldpf :: [Integer] -> Integer -> Integer
ldpf [] _     = 0
ldpf (p:ps) n | rem n p == 0 = p
              | (p^2) > n    = n
              | otherwise    = ldpf ps n

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ldp n

primes :: [Integer]
primes = 2 : filter prime [3..]

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