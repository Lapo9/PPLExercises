-- 24/07/2019
module Nfsa where

data State = A | B | C | D deriving (Show)

-- example automaton: a* (bd)* b [c | (f* (gh)*)* g]
transition :: Char -> State -> [State]
transition 'a' A = [A]
transition 'b' A = [B, C]
transition 'c' B = [D]
transition 'd' B = [A]
transition 'f' C = [C]
transition 'g' C = [D]
transition 'h' D = [C]
transition _ _ = []

end :: State -> Bool
end A = False
end B = False
end C = False
end D = True

start :: [State]
start = [A]

data Config = Config String [State] deriving (Show)

accepts :: String -> [State] -> Bool
accepts "" states = foldr (\s acc -> acc || end s) False states
accepts (c:ss) states = accepts ss (states >>= (transition c))
