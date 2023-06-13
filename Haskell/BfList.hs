-- 08/02/2019

module BfList where

data BfList a = BfList Bool [a]

instance (Show a) => Show (BfList a) where
  show (BfList True l) = "+" ++ show l
  show (BfList False l) = "-" ++ show l

instance (Eq a) => Eq (BfList a) where
  (==) (BfList dir1 l1) (BfList dir2 l2) = dir1 == dir2 && l1 == l2

(<++>) :: BfList a -> BfList a -> BfList a
(<++>) (BfList dir1 []) (BfList dir2 []) = BfList True []
(<++>) (BfList dir1 []) bfl2 = bfl2
(<++>) bfl1 (BfList dir2 []) = bfl1
(<++>) (BfList dir1 (l1:ls1)) (BfList dir2 (l2:ls2)) 
  | dir1 == dir2 = BfList dir1 ((l1:ls1) ++ (l2:ls2))
  | otherwise = (BfList dir1 ls1) <++> (BfList dir2 ls2)

instance Functor BfList where
  fmap func (BfList dir l) = BfList dir (fmap func l)

instance Foldable BfList where
  foldr op v0 (BfList dir l) = foldr op v0 l

bflConcat :: BfList (BfList a) -> BfList a
bflConcat bflbfl = foldr (<++>) (BfList True []) bflbfl

instance Applicative BfList where
  pure v = BfList True [v]
  funcsBfl <*> itemsBfl = bflConcat $ fmap (\func -> fmap func itemsBfl) funcsBfl