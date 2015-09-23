module Problems.G7 where

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
