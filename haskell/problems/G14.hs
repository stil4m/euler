module Problems.G14 where

import UTIL
import Data.List

-- 135. Same differences
euler135 :: Integer
euler135 =  toInteger $ length (filter (== 10) $ map (length . primeCenterCandidates) ( filter (not.prime) [1100..10^6]))

primeCenterCandidates :: Integer -> [Integer]
primeCenterCandidates n = [x | x <- myDivs n, let y = div n x, rem (x + y) 4 == 0, div (x + y) 4 < x]

myDivs :: Integer -> [Integer]
myDivs n = nub $ concat [ [x,y] | x <- [1..limit], rem n x == 0, let y = div n x] where
    limit = (floor.sqrt.fromIntegral) n

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]
