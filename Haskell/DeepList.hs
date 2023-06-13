-- 20/07/2018

module DeepList where

data DeepList a = Inner [a] | Outer a (DeepList a) a

instance (Show a) => Show (DeepList a) where
  show (Inner xs) = " " ++ show xs ++ " "
  show (Outer a xs z) = " [" ++ (show a) ++ (show xs) ++ (show z) ++ "] "

fep :: [a] -> DeepList a
fep xs = foldr (\x acc -> Outer x acc x) (Inner xs) xs

instance Functor DeepList where
  fmap func (Inner l) = Inner (fmap func l)
  fmap func (Outer a l z) = Outer (func a) (fmap func l) (func z)