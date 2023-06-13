-- 07/02/2020
module PriceList where

data PriceList a = PriceList [(a, Float)] deriving (Show)

instance Functor PriceList where
  fmap f (PriceList l) = PriceList (fmap (\(o, p) -> (f o, p)) l)

instance Foldable PriceList where
  foldr f i (PriceList l) = foldr (\(o, _) acc -> f o acc) i l

instance Applicative PriceList where
  pure o = PriceList [(o, 0)]
  (PriceList f) <*> (PriceList l) = PriceList (map (\(func, increment) -> (\(o, p) -> (func o, increment + p))) f <*> l)