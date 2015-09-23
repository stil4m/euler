module Problems.G4
where

import Data.List
import UTIL
import ONeillPrimes
import Data.Ratio
import Data.Char
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
euler33 :: Integer
euler33 = denominator $ foldr ((\(a,b) z -> z * toRational a / toRational b) . fst) 1.0 matchingFractions

fractions :: [((Integer,Integer),(Integer,Integer))]
fractions = [((div x 10, rem y 10),(x,y)) | x <- [10..89], y <- [(x+1)..99]]

validFractions :: [((Integer,Integer),(Integer,Integer))]
validFractions = filter (\((a,b), (c,d)) -> (last (show c) == head (show d)) && a/= 0 && b /= 0) fractions

matchingFractions :: [((Integer,Integer),(Integer,Integer))]
matchingFractions = filter (\((a, b),(c,d)) -> fromInteger a / fromInteger b == fromInteger c/fromInteger d) validFractions


-- 34. Digit factorials
euler34 :: Integer
euler34 = sum $ filter (\q -> q == digitFactorial q) [3..2540160]

naturalList :: Integer -> [Integer]
naturalList 0 = []
naturalList x = rem x 10 : naturalList (div x 10)

digitFactorial :: Integer -> Integer
digitFactorial x = sum $ map factorial $ naturalList x



-- 35. Circular primes
euler35 :: Integer
euler35 = toInteger $ length $ filter circularPrime $ takeWhile (<1000000) primes

digitRotation :: Integer -> [Integer]
digitRotation n = digitRot (length $ show n) $ show n
    where
    digitRot :: Int -> String -> [Integer]
    digitRot _ [] = []
    digitRot l (x:xs) = if l == 0 then [] else (read (x:xs) :: Integer) : digitRot (l-1) (xs ++ [x])

circularPrime :: Integer -> Bool
circularPrime x = all prime (digitRotation x)


-- 36. Double-base palindromes
euler36 :: Integer
euler36 = sum $ filter (\q -> isPalindrome q && isPalindrome (toBin q)) [1..10^6]

-- 37. Truncatable primes
euler37 :: Integer
euler37 = sum truncablePrimes

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
euler39 :: Integer
euler39 = snd $ maximum $ map (\q -> (length $ rightTrianglesPerimeter q, q)) [1..1000]

fracToInt :: RealFrac a => a -> Integer
fracToInt x = toInteger $ floor x

rightTrianglesPerimeter :: Integer -> [(Integer, Integer, Integer)]
rightTrianglesPerimeter n = [ (x,y,z) | x <- [1..(div n 2)], y <- [(x+1)..(div (n-x) 2)], let z = n - x - y, x^2 + y^2 == z^2]


-- 40. Champernowne's constant
euler40 :: Int
euler40 = product $ map digitToInt (findIndexes locations $ champernowneSeq [1..])
    where
    locations  = map ((\q -> q-1) . (10^)) [0..6]

champernowneSeq :: [Integer] -> String
champernowneSeq = foldr ((++) . show) []

findIndexes :: [Integer] -> [a] -> [a]
findIndexes [] _ = []
findIndexes (x:xs) ys = head (take 1 $ drop (fromIntegral x) ys) : findIndexes (map (\q -> q-x :: Integer) xs) (drop (fromIntegral x) ys)
