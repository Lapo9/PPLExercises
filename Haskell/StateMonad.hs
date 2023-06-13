module StateMonad where

data StateProc s a = StateProc (s -> (a, s))

runStateProc :: StateProc s a -> s -> (a, s)
runStateProc (StateProc f) s = f s

instance Functor (StateProc s) where
  fmap g (StateProc f) = StateProc (\s -> let (o1, s1) = f s
                                           in (g o1, s1)
                            )

instance Applicative (StateProc s) where
  pure x = StateProc (\s -> (x, s))
  (StateProc ff) <*> (StateProc ef) = StateProc(\s -> let (outFunc, s1) = ff s
                                                          (outElem, s2) = ef s1
                                                       in (outFunc outElem, s2)
                                    )

instance Monad (StateProc s) where
  return = pure
  (StateProc f) >>= g = StateProc(\s -> let (o1, s1) = f s
                                            StateProc f1 = g o1
                                         in f1 s1
                        )
                                  