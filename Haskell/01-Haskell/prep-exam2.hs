--2016-01

setUnion :: Ord a => [a] -> [a] -> [a]
setUnion [] x = x
setUnion x [] = x
setUnion (x:xs) (y:ys)
  | x == y    = (x:(setUnion xs ys))
  | x < y     = (x:(setUnion xs (y:ys)))
  | otherwise = (y:(setUnion (x:xs) ys))

setIntersect :: Ord a => [a] -> [a] -> [a]
setIntersect [] _ = []
setIntersect _ [] = []
setIntersect (x:xs) (y:ys)
  | x == y    = (x:(setIntersect xs ys))
  | x < y     = (setIntersect xs (y:ys))
  | otherwise = (setIntersect (x:xs) ys)

setDiff :: Ord a => [a] -> [a] -> [a]
setDiff [] _ = []
setDiff x [] = x
setDiff (x:xs) (y:ys)
  | x == y    = (setDiff xs ys)
  | x < y     = (x:(setDiff xs (y:ys)))
  | otherwise = (setDiff (x:xs) ys)

setSymDiff :: Ord a => [a] -> [a] -> [a]
setSymDiff x y = setDiff (setUnion x y) (setIntersect x y)