notProgression :: Int -> Int -> Int -> Bool
notProgression a b c = b - a /= c - b

a 1 = 1
a 2 = 1
a n = head [ x | x <- [1..], all (\k -> (notProgression (a (n - 2*k)) (a (n - k)) x)) [1..(n `div` 2)]]

forestFire = map a [1..]
