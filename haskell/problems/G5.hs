module Problems.G5
where

import Data.List
import UTIL
import Data.Char
import Data.List.Split
import ONeillPrimes

-- 41. Pandigital prime
panDigitsUpTo :: Int -> [Integer]
panDigitsUpTo n = map (\q -> read q :: Integer) $ sortBy (flip compare) $ permutations (map intToDigit [1..n])

euler41 :: Integer
euler41 = head $ take 1 $ filter prime $ concatMap panDigitsUpTo $ reverse [1..9]


-- 42. Coded triangle numbers
-- cabal install split
euler42 :: IO ()
euler42 = do
  s <- readFile "p042_words.txt"
  let t = length $ filter isTriangularNum $ map (wordToSum) (euler42fileWords s) where
  print t

euler42fileWords :: String -> [String]
euler42fileWords s = map (tail.init) (splitOn "," s)

triangleNumbers :: [Int]
triangleNumbers = triangleNumb [1..]
    where
    triangleNumb (x:xs) = floor(x*(x+1)/2) : triangleNumb xs

isTriangularNum :: Int -> Bool
isTriangularNum n = firstMatch == n where
    firstMatch = head $ dropWhile (<n) triangleNumbers

-- 43. Sub-string divisibility
euler43 :: Integer
euler43 = sum $ map ((\q -> read q :: Integer) . concatMap show . uncurry (++)) handlePrimes

euler43start :: [([Integer], [Integer])]
euler43start = map (\p -> (p, [])) [[x,y] | x <- [0..9], y <- [0..9], x/=y]

handlePrimes :: [([Integer], [Integer])]
handlePrimes = foldr handlePrime euler43start (1: take 7 primes) -- Add 1 for to fix the last number

handlePrime :: Integer -> [([Integer], [Integer])] -> [([Integer], [Integer])]
handlePrime p = concatMap (uncurry (filterWithDivisor p))

filterWithDivisor :: Integer -> [Integer] -> [Integer] -> [([Integer],[Integer])]
filterWithDivisor n xs ys = map (\x -> (x: init xs, last xs : ys)) candidates where
  candidates = [x | x<- [0..9], x `notElem` xs, x `notElem` ys, divides (intListToSingle (x:xs)) n]

intListToSingle :: [Integer] -> Integer
intListToSingle = foldl (\b a -> b * 10 + a) 0


-- 44. Pentagon numbers
euler44 :: Integer
euler44 = head $ map (\(p,q) -> p-q ) $take 1 [ (z,a) | x <- [1..], let z = toPentagon x, y <-[1..x], let a = toPentagon y, isPentagon (z - a), isPentagon (z+a) ]

isPentagon :: Integer -> Bool
isPentagon n = isInt ((sqrt(24 * fromInteger n + 1) + 1) / 6)
isInt x = x == fromInteger (round x)


-- 45. Triangular, pentagonal, and hexagonal
toTriangular :: Integer -> Integer
toTriangular x = div (x * (x + 1)) 2

toHexagonal :: Integer -> Integer
toHexagonal x = x * (2 * x - 1)

specialTPHTriplet :: (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]
specialTPHTriplet (t, p, h) | toTriangular t == toPentagon p && toPentagon p == toHexagonal h = (t, p, h) : specialTPHTriplet (t+1, p+1, h+1)
                            | toTriangular t < toPentagon p = specialTPHTriplet (t+1,p,h)
                            | toTriangular t > toPentagon p = specialTPHTriplet (t,p+1,h)
                            | toTriangular t < toHexagonal h = specialTPHTriplet (t+1,p,h)
                            | toTriangular t > toHexagonal h = specialTPHTriplet (t,p,h+1)
                            | toPentagon p < toHexagonal h = specialTPHTriplet (t,p+1,h)
                            | otherwise = specialTPHTriplet (t,p,h +1)

euler45 :: Integer
euler45 = head $ map (\(x,_,_) -> toTriangular x) $ specialTPHTriplet (286,166,144)


-- 46. Goldbach's other conjecture
oddComposites :: [Integer]
oddComposites = drop 1 $ filter (not .prime) $ filter odd [1..]

isGoldbach :: Integer -> Bool
isGoldbach x = any isSquare $ map (\q -> div (x-q) 2) $ takeWhile (<x) $ drop 1 primes

euler46 :: Integer
euler46 = head $ filter (not . isGoldbach) oddComposites

-- 47. Distinct primes factors
euler47 :: Integer
euler47 = euler47sol (5, factors 2, factors 3, factors 4)

euler47sol :: (Integer,[Integer],[Integer],[Integer]) -> Integer
euler47sol (n, x, y, z) = if result then (n - 3) else euler47sol (n+1, y, z, factors n) where
  result = all (not.prime) [n-2..n] && all (\b -> length (nub b) == 4) [x, y, z, baz]
    where
      baz :: [Integer]
      baz = factors n


-- 48. Self powers
euler48 :: Integer
euler48 = read (reverse $ take 10 $ reverse $ show (sum $ map (\x -> x^x) [1..1000])) :: Integer

-- 49. Prime permutations
fourDigitPrimes :: [Integer]
fourDigitPrimes = dropWhile (<1000) $ takeWhile (<(10000- 2*3330)) primes

fourDigitPrimeTriplets :: [(Integer,Integer,Integer)]
fourDigitPrimeTriplets = filter (\(_, y, z) -> prime y && prime z) $ map (\x -> (x, x+3330, x + 6660)) fourDigitPrimes

integerPermutation :: Integer -> Integer -> Bool
integerPermutation x y = sort (show x) == sort (show y)

findPrimePermutationTriplets :: [(Integer, Integer, Integer)]
findPrimePermutationTriplets = filter(\(x, y, z) -> integerPermutation x  y && integerPermutation x z) fourDigitPrimeTriplets

euler49 :: Integer
euler49 = last $ map (\(x, y, z) -> x * 100000000 + y * 10000 + z) findPrimePermutationTriplets

-- 50. Consecutive prime sum
euler50 = euler50' 1000000 (0,0,primes) where
  euler50' n = whiler
    (\ (x, _, ps) -> head ps * x < n)
    (euler50exec n)
    (\ (x, y, _) -> (x,y))
  euler50exec n (x, y, ps) = if x < fst result then (fst result, snd result, tail ps) else (x, y, tail ps)
    where result = consecutivePrimes (<n) ps

consecutivePrimes :: (Integer -> Bool) -> [Integer] -> (Integer, Integer)
consecutivePrimes prop xs = (toInteger $ length result, head result) where
  result = dropWhile (\q -> (not.prime) q || (not.prop) q) (consecutive prop ([],xs))

consecutive prop = whiler
          (\ (xs,ps) -> null xs || ((not.null) ps && prop (head xs) && prop (head ps)))
          (\ (xs,ps) -> if null xs then ([head ps], tail ps) else ((head ps + head xs) : xs, tail ps))
          fst
