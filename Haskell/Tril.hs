-- 03/09/2019
module Tril where

data Tril a = Tril {l1 :: [a], l2 :: [a], l3 :: [a]} deriving (Show)

list2tril :: [a] -> Int -> Int -> Tril a
list2tril l x y
  | x > y = list2tril l y x
  | otherwise =
      let (l1, rest) = split (length l - (x + (y - x))) l
          (l2, l3) = split (y - x) rest
       in Tril l1 l2 l3

split :: Int -> [a] -> ([a], [a])
split n l
  | n >= length l = (l, [])
  | n <= 0 = ([], l)
split n (x : xs) = let (h, t) = split (n - 1) xs in ([x] ++ h, t)

instance Functor Tril where
  fmap f (Tril l1 l2 l3) = Tril (map f l1) (map f l2) (map f l3)

instance Foldable Tril where
  foldr f acc (Tril l1 l2 l3) =
    let folded3 = foldr f acc l3
        folded2 = foldr f folded3 l2
        folded1 = foldr f folded2 l1
     in folded1

(+++) :: Tril a -> Tril a -> Tril a
(Tril a1 a2 a3) +++ (Tril b1 b2 b3) = Tril (a1 ++ a2) (a3 ++ b1) (b2 ++ b3)

concatTril :: (Foldable fold) => fold (Tril a) -> Tril a
concatTril ft = foldr (+++) (Tril [] [] []) ft

concatMapTril :: (Functor f, Foldable f) => (a -> Tril b) -> f a -> Tril b
concatMapTril func t = concatTril (fmap func t)

instance Applicative Tril where
  pure x = Tril [x] [] []
  fs <*> ls = concatMapTril (\f -> fmap f ls) fs
