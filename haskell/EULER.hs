module EULER where

import UTIL
import INPUT
import Data.Char
import Data.List
import ONeillPrimes
import Data.Ratio
import Data.String

-- 1. Multiples of 3 and 5
euler1 :: Integer
euler1 = sum $ filter (\x -> rem x 3  == 0 || rem x 5 == 0) [1..999]

-- 2. Even Fibonacci numbers

fib :: Integer -> Integer -> Integer -> [Integer]
fib x y m | x + y >= m =  []
            | otherwise = (x + y) : fib y (x + y) m

fibonaci :: Integer -> [Integer]
fibonaci = fib 0 1

euler2 :: Integer
euler2 = sum (filter even (fibonaci 4000000))

-- 3. Largest prime factor
euler3 :: Integer
euler3 = last (factors 600851475143)

-- 4. Largest palindrome product
isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)
euler4 :: Integer
euler4 = maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

-- 5. Smallest multiple
smallestMultiple :: Integer -> [Integer]
smallestMultiple x
    | x == 0 = []
    | rem (product sub) x == 0 = 1 : sub
    | otherwise = ldp x : sub
    where sub = smallestMultiple (x- 1)
euler5 :: Integer
euler5 = product (smallestMultiple 20)

-- 6. Sum square difference
sumSquareDiff :: Integer -> Integer
sumSquareDiff x = sum [1..x]^2 - sum (map (^2) [1..x])
euler6 :: Integer
euler6 = sumSquareDiff 100

-- 7. 10001st prime
euler7 :: Integer
euler7 = last (take 10001 primes)

-- 8. Largest product in a series
longestStreak :: Int -> [Int] -> [Int]
longestStreak _ [] = []
longestStreak n xs = product (take n xs) : longestStreak n (tail xs)
euler8 :: Integer
euler8 = toInteger $ maximum (longestStreak 13 (map digitToInt euler8input) )

-- 9. Special Pythagorean triplet
euler9 :: Integer
euler9 = product [a, b, c]
    where (a, b, c) = head [(x, y, z) | x <- [1..333], y <- [a..500], let z = 1000 - x - y, a^2 + b^2 == c^2 ]

-- 10. Summation of primes
euler10 :: Integer
euler10 = sum (takeWhile (<2000000) primes)

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

-- 32. Pandigital products
pandigital :: String -> Bool
pandigital x = sort x == "123456789"

isPandigital :: (Integer, Integer, Integer) -> Bool
isPandigital (x, y,z) = pandigital (show x ++ show y ++ show z)

pandigitalTriples :: [(Integer, Integer, Integer)]
pandigitalTriples = filter isPandigital [(x,y,z) | x <- [1..1987], y <- [1..(x-1)], let z = x * y ]

euler32 :: Integer
euler32 = sum $ rmdups [ z | (_,_,z) <- pandigitalTriples]

-- 33. Digit cancelling fractions
fractions :: [((Integer,Integer),(Integer,Integer))]
fractions = [((div x 10, rem y 10),(x,y)) | x <- [10..89], y <- [(x+1)..99]]

validFractions :: [((Integer,Integer),(Integer,Integer))]
validFractions = filter (\((a,b), (c,d)) -> (last (show c) == head (show d)) && a/= 0 && b /= 0) fractions

matchingFractions :: [((Integer,Integer),(Integer,Integer))]
matchingFractions = filter (\((a, b),(c,d)) -> fromInteger a / fromInteger b == fromInteger c/fromInteger d) validFractions

euler33 :: Integer
euler33 = denominator $ foldr ((\(a,b) z -> z * toRational a / toRational b) . fst) 1.0 matchingFractions

-- 34. Digit factorials
naturalList :: Integer -> [Integer]
naturalList 0 = []
naturalList x = rem x 10 : naturalList (div x 10)

digitFactorial :: Integer -> Integer
digitFactorial x = sum $ map factorial $ naturalList x

euler34 :: Integer
euler34 = sum $ filter (\q -> q == digitFactorial q) [3..2540160]


-- 35. Circular primes
digitRotation :: Integer -> [Integer]
digitRotation n = digitRot (length $ show n) $ show n
    where
    digitRot :: Int -> String -> [Integer]
    digitRot _ [] = []
    digitRot l (x:xs) = if l == 0 then [] else (read (x:xs) :: Integer) : digitRot (l-1) (xs ++ [x])

circularPrime :: Integer -> Bool
circularPrime x = all prime (digitRotation x)


euler35 :: Integer
euler35 = toInteger $ length $ filter circularPrime $ takeWhile (<1000000) primes


-- 36. Double-base palindromes
euler36 :: Integer
euler36 = sum $ filter (\q -> isPalindrome q && isPalindrome (toBin q)) [1..10^6]

-- 37. Truncatable primes
truncs :: Integer -> [Integer]
truncs n = map (\q -> read q :: Integer) (asTruncs (show n) ++ asTruncsR (init $ show n))
    where
    asTruncs :: String -> [String]
    asTruncs []     = []
    asTruncs (x:xs) = (x:xs) : asTruncs xs
    asTruncsR :: String -> [String]
    asTruncsR [] = []
    asTruncsR xs = xs : asTruncsR (init xs)

truncablePrimes :: [Integer]
truncablePrimes = take 11 $ filter (all prime . truncs) $ filter (\q -> rem q 10 `elem` [3,5,7]) (dropWhile (<10) primes)

euler37 :: Integer
euler37 = sum truncablePrimes


--38. Pandigital multiples
pandigitalScope :: Integer -> [Integer]
pandigitalScope 2 = [1000..9999]
pandigitalScope 3 = [100..333]
pandigitalScope 4 = [10..33]
pandigitalScope 5 = [2..19]
pandigitalScope _ = [1..9]

euler38 :: Integer
euler38 = maximum $ map (\q -> read q :: Integer) pandigitalMultiples

pandigitalMultiples :: [String]
pandigitalMultiples = filter pandigital items
    where
    items = concatMap concatMultiplication scopeMapping
    scopeMapping = [ ([1..x], y) | x <- [2..9], let y = pandigitalScope x ]
    concatMultiplication (x,y) = map (\p -> concatMap (\q -> show $ p*q) x) y


-- 39. Integer right triangles
fracToInt :: RealFrac a => a -> Integer
fracToInt x = toInteger $ floor x

rightTrianglesPerimeter :: Integer -> [(Integer, Integer, Integer)]
rightTrianglesPerimeter n = [ (x,y,z) | x <- [1..(div n 2)], y <- [(x+1)..(div (n-x) 2)], let z = n - x - y, x^2 + y^2 == z^2]

euler39 :: Integer
euler39 = snd $ maximum $ map (\q -> (length $ rightTrianglesPerimeter q, q)) [1..1000]

-- 40. Champernowne's constant
champernowneSeq :: [Integer] -> String
champernowneSeq = foldr ((++) . show) []

findIndexes :: [Integer] -> [a] -> [a]
findIndexes [] _ = []
findIndexes (x:xs) ys = head (take 1 $ drop (fromIntegral x) ys) : findIndexes (map (\q -> q-x :: Integer) xs) (drop (fromIntegral x) ys)

euler40 :: Int
euler40 = product $ map digitToInt (findIndexes locations $ champernowneSeq [1..])
    where
    locations  = map ((\q -> q-1) . (10^)) [0..6]


-- 41. Pandigital prime
panDigitsUpTo :: Int -> [Integer]
panDigitsUpTo n = map (\q -> read q :: Integer) $ sortBy (flip compare) $ permutations (map intToDigit [1..n])

euler41 :: Integer
euler41 = head $ take 1 $ filter prime $ concatMap panDigitsUpTo $ reverse [1..9]


-- 42. Coded triangle numbers
triangleNumbers :: [Integer]
triangleNumbers = triangleNumb [1..]
    where
    triangleNumb (x:xs) = floor(x*(x+1)/2) : triangleNumb xs

isTriangularNum :: Integer -> Bool
isTriangularNum n = checkTriangularNum n triangleNumbers where
    checkTriangularNum :: Integer -> [Integer] -> Bool
    checkTriangularNum _ []  = False
    checkTriangularNum x (y:ys) | x < y = False
                                | x == y = True
                                | otherwise = checkTriangularNum x ys

--stringToNumber :: [Char] -> [Int]
--stringToNumber


-- 44. Pentagon numbers
toPentagon :: Integer -> Integer
toPentagon x =  div (x * (3 * x - 1)) 2

pentagons :: [Integer]
pentagons = map toPentagon [1..]

euler44 :: Integer
euler44 = undefined


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

-- 67. Maximum path sum II
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

euler67 :: IO ()
euler67 = do
       content <- readFile "p067_triangle.txt"
       let linesOfFile = computeMaxPath content
       print linesOfFile

-- 119. Digit power sum

euler119 = (dropWhile (<=10) $ sort $ map snd $ concatMap factorValues [1..1000]) !! 29
factorValues :: Integer -> [(Integer, Integer)]
factorValues n = filter (\(p,_) -> p == n) $ map ((\q -> (digitSum q, q)) . (n^)) [1..100]

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


eulerAll :: [(Integer, Integer)]
eulerAll = [(1, euler1), (2, euler2), (3, euler3)
            , (3, euler3)
            , (4, euler4)
            , (5, euler5)
            , (6, euler6)
            , (7, euler7)
            , (8, euler8)
            , (9, euler9)
            , (10, euler10)
            , (13, euler13)
--            , (14, euler14) long running
            , (15, euler15)
            , (32, euler32)
            , (33, euler33)
            , (49, euler49)
            ]
