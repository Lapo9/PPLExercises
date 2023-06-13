-- calculates each bmi in a weight,height list
module Bmi where

bmi :: (Floating a) => [(a, a)] -> [a]
bmi l = [w / h ^ 2 | (w, h) <- l]

bmi2 :: (Floating a) => [(a, a)] -> [a]
bmi2 l = [bmi w h | (w, h) <- l]
  where bmi weight height = weight / height ^ 2