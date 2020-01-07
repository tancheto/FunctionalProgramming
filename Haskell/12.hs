module BinaryTree (BinaryTree(Empty, Node), root, left, right, isEmpty) where

data BinaryTree t = Empty | Node { root :: t,
                                   left :: BinaryTree t,
                                   right :: BinaryTree t }
                    deriving (Eq, Ord, Show, Read)

isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty _ = False

--11-06

bloom :: BinaryTree t -> BinaryTree t
bloom Empty = Empty
bloom t@(Node x Empty Empty) = Node x t t
bloom (Node x l r) = Node x (bloom l) (bloom r)

--11-07

prune :: BinaryTree t -> BinaryTree t
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node x l r) = Node x (prune l) (prune r)

--12-01

type BST = BinaryTree

bstInsert :: Ord t => BST t -> t -> BST t
bstInsert Empty x = Node x Empty Empty
bstInsert t@(Node x l r) x'
  | x' < x    = Node x (bstInsert l x') r
  | x' > x    = Node x l (bstInsert r x')
  | otherwise = t

bstSearch :: Ord t => BST t -> t -> Bool
bstSearch Empty _ = False
bstSearch (Node x l r) x'
  | x' == x   = True
  | x' < x    = bstSearch l x'
  | otherwise = bstSearch r x'

bstSize :: Ord t => BST t -> Int
bstSize Empty = 0
bstSize (Node _ l r) = 1 + bstSize l + bstSize r

bstFromList :: Ord t => [t] -> BST t
bstFromList = foldl bstInsert Empty

bstValues :: BST t -> [t]
bstValues Empty = []
bstValues (Node x l r) = bstValues l ++ x : bstValues r

bstSort :: Ord t => [t] -> [t]
bstSort = bstValues . bstFromList

data Direction = L | R deriving Show

bstPath :: (Ord t) => BST t -> t -> Maybe [Direction]
bstPath Empty _ = Nothing
bstPath (Node x l r) x'
  | x' == x   = Just []
  | x' < x    = navigate (bstPath l x') L
  | otherwise = navigate (bstPath r x') R
  where navigate Nothing _ = Nothing
        navigate (Just ds) d = Just (d : ds)

type Map k v = BinaryTree (k, v)

mapInsert :: Ord k => Map k v -> k -> v -> Map k v
mapInsert Empty k v = Node (k, v) Empty Empty
mapInsert (Node (k, v) l r) k' v'
  | k' < k = Node (k, v) (mapInsert l k' v') r
  | k' > k = Node (k, v) l (mapInsert r k' v')
  | otherwise = Node (k, v') l r

mapSearch :: Ord k => Map k v -> k -> Maybe v
mapSearch Empty _ = Nothing
mapSearch (Node (k, v) l r) k'
  | k' < k = mapSearch l k'
  | k' > k = mapSearch r k'
  | otherwise = Just v

intervalTree :: (Num t, Ord t) => BinaryTree t -> BinaryTree (t, t)
intervalTree Empty = Empty
intervalTree (Node x l r) = Node (interval x l' r') l' r'
  where l' = intervalTree l
        r' = intervalTree r
        interval x Empty Empty = (x, x)
        interval x (Node (y', y'') _ _) Empty = (min x y', max x y'')
        interval x Empty (Node (y', y'') _ _) = (min x y', max x y'')
        interval x (Node (y', y'') _ _) (Node (z', z'') _ _) =
        	(minimum [x, y', z'], maximum [x, y'', z''])










