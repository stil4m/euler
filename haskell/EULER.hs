module EULER

where
import UTIL
import INPUT
import Data.String
import Data.Char
import GHC.Float
import Data.List
import ONeillPrimes
import Data.Ratio

-- 1. Multiples of 3 and 5
euler1 :: Integer
euler1 = sum $ filter (\x -> rem x 3  == 0 || rem x 5 == 0) [1..999]

-- 2. Even Fibonacci numbers

fib x y max | x + y >= max =  []
            | otherwise = (x + y) : fib y (x + y) max
fibonaci = fib 0 1
euler2 :: Integer
euler2 = sum (filter even (fibonaci 4000000))

-- 3. Largest prime factor
euler3 :: Integer
euler3 = last (factors 600851475143)

-- 4. Largest palindrome product
isPalindrome :: Integer -> Bool
isPalindrome x = (show x) == reverse (show x)
euler4 :: Integer
euler4 = maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

-- 5. Smallest multiple
smallestMultiple :: Integer -> [Integer]
smallestMultiple x
    | x == 0 = []
    | (rem (product sub) x) == 0 = 1 : sub
    | otherwise = (ldp x) : sub
    where sub = (smallestMultiple (x- 1))
euler5 :: Integer
euler5 = product (smallestMultiple 20)

-- 6. Sum square difference
sumSquareDiff x = (sum [1..x])^2 - (sum (map (^2) [1..x]))
euler6 :: Integer
euler6 = sumSquareDiff 100

-- 7. 10001st prime
euler7 :: Integer
euler7 = last (take 10001 primes)

-- 8. Largest product in a series
longestStreak :: Int -> [Int] -> [Int]
longestStreak _ [] = []
longestStreak n xs = (product (take n xs)) : longestStreak n (tail xs)
euler8 :: Integer
euler8 = toInteger $ maximum (longestStreak 13 (map digitToInt euler8input) )

-- 9. Special Pythagorean triplet
euler9 :: Integer
euler9 = toInteger $ product [float2Int a, float2Int b,float2Int  c]
    where (a, b, c) = head [(a, b, sqrt(a^2 + b^2) ) | a <- [1..500], b <- [1..500], a < b, (a + b + sqrt(a^2 + b^2) == 1000) ]

-- 10. Summation of primes
euler10 :: Integer
euler10 = sum (takeWhile (<2000000) primes)

-- 11. Largest product in a grid

-- 12. Highly divisible triangular number

-- 13. Large sum
euler13 :: Integer
euler13 = read (take 10 $ show $ sum euler13input) :: Integer

 -- 14. Longest Collatz sequence
collatz_sequence = run
collatz_sequences :: [Integer] -> [[Integer]]
collatz_sequences x = (map collatz_sequence x)
max_collatz_sequence = mxmInt (map length (collatz_sequences [1..999999]))
euler14 :: Integer
euler14 = toInteger max_collatz_sequence

-- 15. Lattice paths
--https://wiki.haskell.org/Blow_your_mind
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]
euler15 = head $ drop 20 $ last (take 41 pascal)

-- 32. Pandigital products
pandigital :: [Char] -> Bool
pandigital x = sort x == "123456789"

isPandigital :: (Integer, Integer, Integer) -> Bool
isPandigital (x, y,z) = pandigital (show x ++ show y ++ show z)
pandigitalTriples = filter isPandigital [(x,y,z) | x <- [1..1987], y <- [1..(x-1)], let z = x * y ]
euler32 = sum $ rmdups [ z | (_,_,z) <- pandigitalTriples]

-- 33. Digit cancelling fractions
fractions = [((div x 10, rem y 10),(x,y)) | x <- [10..89], y <- [(x+1)..99]]
validFractions = filter (\((a,b), (c,d)) -> ((last $ show c) == (head $ show d)) && a/= 0 && b /= 0) fractions
matchingFractions = filter (\((a, b),(c,d)) -> (fromInteger a)/(fromInteger b) == (fromInteger c)/(fromInteger d)) validFractions
euler33 = denominator $ foldr (\(a,b) z -> z * (toRational a) / (toRational b)) 1.0 $ map (\(x,_) -> x) matchingFractions

-- 34. Digit factorials
naturalList :: Integer -> [Integer]
naturalList 0 = []
naturalList x = rem x 10 : (naturalList $ div x 10)

digitFactorial :: Integer -> Integer
digitFactorial x = sum $ map (\q -> factorial q) $ naturalList x

euler34 = sum $ filter (\q -> q == digitFactorial q) [3..2540160]


-- 35. Circular primes
digitRotation :: Integer -> [Integer]
digitRotation x = digitRot (length $ show x) $ show x
    where
    digitRot :: Int -> [Char] -> [Integer]
    digitRot l (x:xs) = if l == 0 then [] else (read (x:xs) :: Integer) : digitRot (l-1) (xs ++ [x])

circularPrime x = and $ map prime $ digitRotation x

euler35 = length $ filter circularPrime $ takeWhile (<1000000) primes


-- 36. Double-base palindromes
euler36 = sum $ filter (\q -> (isPalindrome q) && (isPalindrome $ toBin q)) [1..10^6]

-- 37. Truncatable primes
truncs :: Integer -> [Integer]
truncs x = map (\q -> read q :: Integer) ((asTruncs $ show x) ++ (asTruncsR $ init $ show x))
    where
    asTruncs :: [Char] -> [[Char]]
    asTruncs []     = []
    asTruncs (x:xs) = (x:xs) : asTruncs xs
    asTruncsR :: [Char] -> [[Char]]
    asTruncsR [] = []
    asTruncsR xs = xs : (asTruncsR $ init xs)

truncablePrimes = take 11 $ filter (\q -> and (map prime $ truncs q) ) $ filter (\q -> (rem q 10) `elem` [3,5,7]) (dropWhile (<10) primes)
euler37 = sum truncablePrimes


--38. Pandigital multiples
pandigitalScope :: Integer -> [Integer]
pandigitalScope 2 = [1000..9999]
pandigitalScope 3 = [100..333]
pandigitalScope 4 = [10..33]
pandigitalScope 5 = [2..19]
pandigitalScope _ = [1..9]


euler38 = last $ sort $ map (\q -> read q :: Integer) pandigitalMultiples
pandigitalMultiples = filter pandigital items
    where
    items = foldr (++) [] $ map concatMultiplication scopeMapping
    scopeMapping = [ ([1..x], y) | x <- [2..9], let y = pandigitalScope x ]
    concatMultiplication (x,y) = map (\p -> foldl (++) [] $ map (\q -> show $ p*q) x) y


-- 39. Integer right triangles
fracToInt :: RealFrac a => a -> Integer
fracToInt x = toInteger $ floor x

rightTrianglesPerimeter :: Integer -> [(Integer, Integer, Integer)]
rightTrianglesPerimeter n = [ (x,y,z) | x <- [1..(div n 2)], y <- [(x+1)..(div (n-x) 2)], let z = n - x - y, x^2 + y^2 == z^2]
euler39 = head $ reverse $ sort $ map (\q -> (length $ rightTrianglesPerimeter q, q)) [1..1000]

-- 40. Champernowne's constant
champernowneSeq :: [Integer] -> [Char]
champernowneSeq [] = []
champernowneSeq (x:xs) = (show x) ++ champernowneSeq xs

findIndexes :: [Integer] -> [a] -> [a]
findIndexes [] _ = []
findIndexes (x:xs) ys = head (take 1 $ drop (fromIntegral x) ys) : findIndexes (map (\q -> q-x :: Integer) xs) (drop (fromIntegral x) ys)

euler40 = product $ map digitToInt (findIndexes locations $ champernowneSeq [1..])
    where
    locations  = map (\q -> q-1) $ map (10^) [0..6]


-- 41. Pandigital prime
panDigitsUpTo :: Int -> [Integer]
panDigitsUpTo n = map (\q -> read q :: Integer) $ reverse $ sort $ permutations (map intToDigit [1..n])
euler41 = head $ take 1 $ filter prime $ foldl (++) [] (map panDigitsUpTo $ reverse [1..9])


-- 42. Coded triangle numbers
triangleNumbers :: [Integer]
triangleNumbers = triangleNumb [1..]
    where
    triangleNumb (x:xs) = floor(x*(x+1)/2) : triangleNumb xs

isTriangularNum :: Integer -> Bool
isTriangularNum x = checkTriangularNum x triangleNumbers where
    checkTriangularNum :: Integer -> [Integer] -> Bool
    checkTriangularNum x (y:ys) =  if (x < y) then False else if x == y then True else checkTriangularNum x ys

--stringToNumber :: [Char] -> [Int]
--stringToNumber


-- 44. Pentagon numbers
toPentagon :: Integer -> Integer
toPentagon x =  (div (x * (3 * x - 1)) 2)

pentagons :: [Integer]
pentagons = _pentagons [1..]
    where
    _pentagons :: [Integer] -> [Integer]
    _pentagons (x:xs) = toPentagon x : _pentagons xs

euler44 = undefined


-- 45. Triangular, pentagonal, and hexagonal
toTriangular :: Integer -> Integer
toTriangular x = (div (x * (x + 1)) 2)

toHexagonal :: Integer -> Integer
toHexagonal x = x * (2 * x - 1)

specialTPHTriplet :: (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]
specialTPHTriplet (t, p, h) | (toTriangular t) == (toPentagon p) && (toPentagon p) == (toHexagonal h) = (t, p, h) : specialTPHTriplet (t+1, p+1, h+1)
                            | (toTriangular t) < (toPentagon p) = specialTPHTriplet (t+1,p,h)
                            | (toTriangular t) > (toPentagon p) = specialTPHTriplet (t,p+1,h)
                            | (toTriangular t) < (toHexagonal h) = specialTPHTriplet (t+1,p,h)
                            | (toTriangular t) > (toHexagonal h) = specialTPHTriplet (t,p,h+1)
                            | (toPentagon p) < (toHexagonal h) = specialTPHTriplet (t,p+1,h)
                            | otherwise = specialTPHTriplet (t,p,h +1)

euler45 = head $ map (\(x,_,_) -> toTriangular x) $ specialTPHTriplet (286,166,144)



-- 49. Prime permutations
fourDigitPrimes :: [Integer]
fourDigitPrimes = dropWhile (<1000) $ takeWhile (<(10000- 2*3330)) primes

fourDigitPrimeTriplets :: [(Integer,Integer,Integer)]
fourDigitPrimeTriplets = filter (\(x, y, z) -> prime y && prime z) $ map (\x -> (x, x+3330, x + 6660)) fourDigitPrimes

integerPermutation :: Integer -> Integer -> Bool
integerPermutation x y = (sort $ show x) == (sort $ show y)
findPrimePermutationTriplets = filter(\(x, y, z) -> integerPermutation x  y && integerPermutation x z) fourDigitPrimeTriplets


euler49 :: Integer
euler49 = last $ map (\(x, y, z) -> x * 100000000 + y * 10000 + z) findPrimePermutationTriplets



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

