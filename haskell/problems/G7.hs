module Problems.G7 where

import UTIL

-- 67. Maximum path sum II
euler67 :: IO ()
euler67 = do
       content <- readFile "p067_triangle.txt"
       let linesOfFile = computeMaxPath content
       print linesOfFile
