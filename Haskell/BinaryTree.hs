module BinaryTree where

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)

instance (Show a) => Show (BinaryTree a) where
  show EmptyTree = "_"
  show (Node e left right) = show e ++ " (" ++ show left ++ ", " ++ show right ++ ") "

instance Functor BinaryTree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

createTree x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insertTree EmptyTree x = createTree x
insertTree tree@(Node y left right) x
  | x == y = tree
  | x > y = Node y left (insertTree right x)
  | x < y = Node y (insertTree left x) right

searchTree :: (Ord a) => BinaryTree a -> a -> Bool
searchTree EmptyTree x = False
searchTree (Node y left right) x
  | x == y = True
  | x > y = searchTree right x
  | x < y = searchTree left x
