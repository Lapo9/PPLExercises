-- 28/06/2019
module Tritree where

data Tritree a = TtEmpty | TtNode a (Tritree a) (Tritree a) (Tritree a)

instance Functor Tritree where
  fmap func TtEmpty = TtEmpty
  fmap func (TtNode v t1 t2 t3) = TtNode (func v) (fmap func t1) (fmap func t2) (fmap func t3)

instance Foldable Tritree where
  foldr func v0 TtEmpty = v0
  foldr func v0 (TtNode v t1 t2 t3) = func v $ foldr func (foldr func (foldr func v0 t3) t2) t1

instance (Show a) => Show (Tritree a) where
  show TtEmpty = "_"
  show (TtNode x t1 t2 t3) = " (" ++ show x ++ ", " ++ show t1 ++ ", " ++ show t2 ++ ", " ++ show t3 ++ ") "

(+++) :: Tritree a -> Tritree a -> Tritree a
TtEmpty +++ tt2 = tt2
tt1 +++ TtEmpty = tt1
(TtNode v t1 t2 TtEmpty) +++ tt2 = TtNode v t1 t2 tt2
(TtNode v t1 t2 t3) +++ tt2 = t3 +++ tt2

ttConcat :: Tritree (Tritree a) -> Tritree a
ttConcat tttt = foldr (+++) TtEmpty tttt

instance Applicative Tritree where
  pure v = TtNode v TtEmpty TtEmpty TtEmpty
  funcsTt <*> elemsTt = ttConcat (fmap (\func -> fmap func elemsTt) funcsTt)