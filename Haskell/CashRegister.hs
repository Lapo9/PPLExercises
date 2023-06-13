-- 15/01/2020
module CashRegister where

data CashRegister a = CashRegister {getReceipt :: (a, Float)} deriving (Show, Eq)

getCurrentItem = fst . getReceipt

getPrice = snd . getReceipt

instance Functor CashRegister where
  fmap f (CashRegister (v, p)) = CashRegister (f v, p)

instance Applicative CashRegister where
  pure c = CashRegister (c, 0)
  (CashRegister (f, x)) <*> (CashRegister (v, y)) = CashRegister (f v, x + y)

instance Monad CashRegister where
  return c = pure c
  (CashRegister (v, p)) >>= f = let (CashRegister (v', p')) = f v in CashRegister (v', p + p')