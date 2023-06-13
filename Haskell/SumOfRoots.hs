-- How many elements does it take for the sum of the roots of all natural numbers to exceed m?
module SumOfRoots where

sumOfRoots m = (i, foldl1 (\x y -> x + sqrt y) [1..fromIntegral i])
  where i = length (takeWhile (< m) (scanl1 (\x y -> x + sqrt y) [1..])) + 1