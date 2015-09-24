module Problems.G10 where

import Data.Char

-- 92. Square digit chains
-- squareDigitChains :: [Integer] -> Map Integer Integer -> Map Integer Integer
-- squareDigitChains (x:xs) m = undefined

-- calculateChain :: [Integer] -> Map Integer Integer -> (Integer, Map Integer Integer)
-- calculateChain k m = if member k m then (Foo.lookup k m, m) else (k, m) where
  -- (n,m) = calculateChain
next k = sum (map ((^2).digitToInt) (show k))


-- 97. Large non-Mersenne prime
euler97 = reverse $ take 10 $ reverse $ show (28433 * 2^7830457 + 1)
