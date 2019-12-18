addAt :: a -> Int -> [a] -> [a]
addAt x n xs = take n xs ++ (x:drop n xs)

------------------------------------------------

mostFrequent :: (Num t, Eq t) => [[t]] -> t
mostFrequent [] = 0
mostFrequent (xs:xss) = if null firstMaxes then 0 else head firstMaxes
  where firstMaxes = filter (\x -> all (\ys -> x `elem` ys) (map mostFrequent' xss)) (mostFrequent' xs)

mostFrequent' :: (Num t, Eq t) => [t] -> [t]
mostFrequent' xs = map fst (filter (\(x, n) -> (n == max)) hist)
  where hist = histogram xs
        max = snd (maximumBy snd hist)

uniq :: Eq t => [t] -> [t]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

histogram :: Eq t => [t] -> [(t, Int)]
histogram xs = map (\x -> (x, occurrencies x xs)) (uniq xs)
  where occurrencies x xs = length (filter (==x) xs)

maximumBy :: Ord a => (t -> a) -> [t] -> t
maximumBy fn xs = foldl1 maxBy xs
  where maxBy x y
          | fn y > fn x = y
          | otherwise = x


