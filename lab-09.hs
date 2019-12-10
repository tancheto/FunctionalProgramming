factorial 0 = 1
factorial x = x * factorial (x-1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

fibonacci' :: Int -> Integer
fibonacci' n = iter 0 1 n
  where iter a b 0 = a
        iter a b n = iter b (a + b) (n - 1)

fibonacci'' :: Int -> Integer
fibonacci'' n = let iter a b 0 = a
                    iter a b n = iter b (a + b) (n - 1)
                in iter 0 1 n 

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci''' :: Int -> Integer
fibonacci''' = (fibs!!)

add :: Int -> (Int -> Int)
add a b = a + b

add5 :: Int -> Int
add5 = add 5

addVectors :: Num t => (t, t) -> (t, t) -> (t, t)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

compress :: Eq t => [t] -> [t]
compress []       = []
compress [x]      = [x]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = x : compress (y:xs)

compress' :: Eq t => [t] -> [t]
compress' []     = []
compress' (x:xs) = x : compress' (dropWhile (==x) xs)

duplicate :: [t] -> [t]
duplicate []     = []
duplicate (x:xs) = x : x : duplicate xs 

cycle' :: [t] -> [t]
cycle' [] = []
cycle' xs = iter xs
  where iter [] = iter xs
        iter (x:xs) = x : iter xs

quickSort :: Ord t => [t] -> [t]
quickSort []           = []
quickSort (pivot:rest) = quickSort lessThanOrEqual ++ [pivot] ++ quickSort largerThan
  where lessThanOrEqual  = filter (<=pivot) rest
        largerThan       = filter (>pivot) rest

mergeSort :: Ord t => [t] -> [t]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort xs = (mergeSort firstHalf) `merge` (mergeSort secondHalf)
  where (firstHalf, secondHalf) = splitAt (length xs `quot` 2) xs
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | y < x     = y : merge (x:xs) ys
          | otherwise = x : merge xs (y:ys)


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (map fst xs, map snd xs)

reverse' :: [a] -> [a]
reverse' []     = []
reverse' [x]    = [x]
reverse' (x:xs) = (reverse' xs) ++ [x] 

iterate' :: (a -> a) -> a -> [a]
iterate' f y = y : (iterate' f (f y))