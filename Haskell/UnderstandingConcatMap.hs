-- Here are shown all the steps to understand concatMap for trees

module UnderstandingConcatMap where

data BinaryTree a = Empty | Leaf a | Node (BinaryTree a) (BinaryTree a)

instance (Show a) => Show (BinaryTree a) where
  show t = " [" ++ showIntern t ++ "] "

showIntern :: (Show a) => BinaryTree a -> String
showIntern Empty = "_"
showIntern (Leaf x) = show x
showIntern (Node left right) = "(" ++ showIntern left ++ ", " ++ showIntern right ++ ")"

tmap :: (a -> b) -> BinaryTree a -> BinaryTree b
tmap f Empty = Empty
tmap f (Leaf x) = Leaf (f x)
tmap f (Node left right) = Node (tmap f left) (tmap f right)

tfoldr :: (a -> b -> b) -> b -> BinaryTree a -> b
tfoldr f x0 Empty = x0
tfoldr f x0 (Leaf x) = f x x0
tfoldr f x0 (Node left right) = tfoldr f (tfoldr f x0 right) left

instance Foldable BinaryTree where
  foldr = tfoldr

tconcat2 :: BinaryTree a -> BinaryTree a -> BinaryTree a
tconcat2 Empty t = t
tconcat2 t Empty = t
tconcat2 t1 t2 = Node t1 t2

tconcat :: BinaryTree (BinaryTree a) -> BinaryTree a
tconcat tt = tfoldr tconcat2 Empty tt

tconcatmap :: ((a -> b) -> BinaryTree b) -> BinaryTree (a -> b) -> BinaryTree b
tconcatmap perFuncFunc funcTree = tconcat (tmap perFuncFunc funcTree)

applConcatmap :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
applConcatmap funcTree valueTree = tconcatmap (\f -> tmap f valueTree) funcTree

applMap :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree (BinaryTree b)
applMap funcTree valueTree = tmap (\f -> tmap f valueTree) funcTree

test = show (applConcatmap
  (Node (Leaf (+1)) (Node (Leaf (*2)) (Leaf (*5))))
  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))