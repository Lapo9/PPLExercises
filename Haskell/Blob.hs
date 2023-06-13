-- 06/07/2018

module Blob where

data Blob a = Blob a (a -> a)

instance Show a => Show (Blob a) where
  show (Blob x f) = "Blob" ++ (show (f x))

instance Eq a => Eq (Blob a) where
  (Blob x1 f1) == (Blob x2 f2) = (f1 x1) == (f2 x2)

instance Functor Blob where
  fmap func (Blob x f) = Blob (func (f x)) id

instance Foldable Blob where
  foldr func v0 (Blob x f) = func (f x) v0

instance Applicative Blob where
  pure x = Blob x id
  (Blob xf ff) <*> e = fmap (ff xf) e