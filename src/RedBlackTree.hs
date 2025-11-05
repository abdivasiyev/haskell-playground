-- As with all binary search trees, the elements in a red-black tree are stored
-- in symmetric order, so that for any node T color a x b, x is greater than
-- any element in a and less than any element in b. In addition, red-black trees
-- satisfy two balance invariants:
--
-- Invariant 1. No red node has a red parent.
-- Invariant 2. Every path from the root to an empty node contains the same number
-- of black nodes.
module RedBlackTree where

-- colors of tree nodes
data Color = Red | Black deriving (Show, Eq, Ord)

-- recursive ADT for red black tree
data Tree a = Empty | Tree Color (Tree a) a (Tree a) deriving (Show, Eq, Ord)

-- Set
type Set a = Tree a

-- empty is a function to return Empty node
empty :: Set a
empty = Empty

-- member is a function to check element is in the Set
member :: (Ord a) => a -> Set a -> Bool
member x Empty = False
member x (Tree _ l y r)
  | x < y = member x l
  | x == y = True
  | x > y = member x r
member _ _ = undefined

-- insert is a function to insert a new node into set without
-- violating red-black tree invariants
insert :: (Ord a) => a -> Set a -> Set a
insert x s = transform Black (insert' s)
  where
    transform _ Empty = Empty
    transform c (Tree _ l y r) = Tree c l y r

    insert' Empty = Tree Red Empty x Empty
    insert' (Tree color left y right)
      | x < y = balance color (insert' left) y right
      | x == y = Tree color left y right
      | x > y = balance color left y (insert' right)
    insert' _ = undefined

balance :: (Ord a) => Color -> Set a -> a -> Set a -> Set a
balance Black (Tree Red (Tree Red a x b) y c) z d = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance Black (Tree Red a x (Tree Red b y c)) z d = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance Black a x (Tree Red (Tree Red b y c) z d) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance Black a x (Tree Red b y (Tree Red c z d)) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance color left x right = Tree color left x right

-- >>> root = insert 5 . insert 10 . insert 3 . insert 12 . insert 7 . insert 1 $ Empty
-- >>> root
-- Tree Black (Tree Red (Tree Black Empty 1 Empty) 3 (Tree Black Empty 5 Empty)) 7 (Tree Black (Tree Red Empty 10 Empty) 12 Empty)
-- >>> member 2 root
-- False
-- >>> member 10 root
-- True
-- >>> member 5 root
-- True
-- >>> member 3 root
-- True
-- >>> member 7 root
-- True
