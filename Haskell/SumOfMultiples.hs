-- Given a number, find the sum of all the unique multiples of particular numbers up to but not including that number.

module SumOfMultiples where

sumOfMultiples :: Int -> Int -> Int -> Int
sumOfMultiples a b m =
  let multOfA = [x | x <- [1 .. m], x `mod` a == 0]
      multOfB = [x | x <- [1 .. m], x `mod` b == 0]
      mult = [x | x <- multOfB, x `notElem` multOfA] ++ multOfA
   in sum mult