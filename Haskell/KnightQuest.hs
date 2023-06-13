module KnightQuest where

type Tile = (Int, Int)

canReachIn :: Int -> Tile -> [Tile] -> Bool
canReachIn moves end start
  | moves == 0 = end `elem` start
  | moves > 0 = if end `elem` start then True else canReachIn (moves - 1) end (start >>= move)
  | otherwise = False

move :: Tile -> [Tile]
move t = [(a, b) | a <- [-1, 1, -2, 2], b <- [-1, 1, -2, 2], abs a /= abs b] >>= sumTiles t

sumTiles :: Tile -> (Int, Int) -> [Tile]
sumTiles (a, b) (c, d) = if x >= 1 && x <= 8 && y >= 1 && y <= 8 then [res] else []
  where
    res@(x, y) = (a + c, b + d)

foo :: (Num a, Ord a) => (a, a) -> (a -> b) -> b
foo pair@(0, _) f =
  let msg = "Some error"
   in error msg
foo n f
  | n > k = f x
  | otherwise = f (y + x)
  where
    (x, y) = n
    k = (2, x)