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
