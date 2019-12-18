--1
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a, b, c) | c <- [1..998], 
                                  b <- [1..(999 - c)],
                                  let a = 1000 - b - c,
                                  a^2 + b^2 == c^2]

--2
divides :: Integral t => t -> t -> Bool
x `divides` y = y `mod` x == 0

divisibleBy :: Integral t => t -> [t] -> Bool
x `divisibleBy` xs = all (`divides` x) xs 

smallestMultiple :: Integral t => t -> t
smallestMultiple n = head [x | x <- [n..], x `divisibleBy` [1..n]] 

--3
isPrime :: Integral t => t -> Bool
isPrime n = all (not . (`divides` n)) [2..floor . sqrt . fromIntegral $ n]

primes :: Integral t => [t]
primes = [x | x <- [2..], isPrime x]

primesLowerThan :: Integral t => t -> [t]
primesLowerThan n = [x | x <- (take (fromIntegral n) primes), x < n]

sumPrimesLowerThan :: Integral t => t -> t
sumPrimesLowerThan = sum . primesLowerThan

primes' :: Integral t => [t]
primes' = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y <- xs, not (x `divides` y)]

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

data Student = Student { name :: String, fn :: Int }

data BinaryTree t = Empty | Node { root :: t,
                                   left :: (BinaryTree t),
                                   right :: (BinaryTree t) }

isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty _ = False

maxSumPath :: (Num t, Ord t) => BinaryTree t -> t
maxSumPath Empty = 0
maxSumPath (Node x l r) = x + max (maxSumPath l) (maxSumPath r)