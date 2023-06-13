-- 05/09/2018

module Dupl where

data Dupl a = Dupl [a] [a] deriving (Eq, Show)

instance Functor Dupl where
  fmap func (Dupl x y) = Dupl (fmap func x) (fmap func y)

instance Foldable Dupl where
  foldr func v0 (Dupl x y) = foldr func (foldr func v0 y) x

instance Applicative Dupl where
  pure x = Dupl [x] []
  (Dupl funcsListX funcsListY) <*> (Dupl elemsListX elemsListY) = Dupl (funcsListX <*> elemsListX) (funcsListY <*> elemsListY)