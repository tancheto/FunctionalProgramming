{-type Item = (String, String, Double)

takeShop :: Item -> String
takeShop (x,_,_) = x

takeCategory :: Item -> String
takeCategory (_,x,_) = x

takePrice :: Item -> Double
takePrice (_,_,x) = x

uniqCat :: [Item] -> [String]
uniqCat [] = []
uniqCat (x:xs) = takeCategory x : (uniqCat (filter (\y -> ((takeCategory y) /= (takeCategory x))) xs))

filterPerCategory :: [Item] -> String -> [Item]
filterPerCategory xs cat = filter (\x -> (cat == (takeCategory x))) xs 

totalPricePerCategory :: [Item] -> String -> Double
totalPricePerCategory xs cat = foldr (+) 0 (map takePrice (filterPerCategory xs cat))

shopMaxPrice :: [Item] -> String -> String
shopMaxPrice xs cat = takeShop (foldr maxBy (head xs) (filterPerCategory xs cat))
  where maxBy x y
          | (takePrice x) < (takePrice y) = y
          | otherwise                     = x

--              category, total price, shop max price
triples :: [Item] -> [(String, Double, String)]
triples xs = map (\cat -> (cat, (totalPricePerCategory xs cat), (shopMaxPrice xs cat))) (uniqCat xs)
-}

Item = (String, String, Double)

takeShop :: Item -> String
takeShop (x,_,_) = x

takeCategory :: Item -> String
takeCategory (_,x,_) = x

takePrice :: Item -> Double
takePrice (_,_,x) = x

commonCategory::[Package] -> [[Package]]
commonCategory [] = []
commonCategory (x:xs) = (filter (\ y -> second y == second x) (x:xs)):commonCategory 
                                                                      (filter (\ y -> second y /= second x) xs)

categoryAllMoney :: [Package] -> Float
categoryAllMoney ls = sum (map (\ y -> third y) ls)

categoryMaxMoney :: [Package] -> String
categoryMaxMoney [] = ""
categoryMaxMoney ls = first (last (sort ls))

sort :: [Package] -> [Package]
sort [] = []
sort (x:xs) = (sort (filter (\ y -> (third y < third x)) xs)) ++ [x] ++ (sort (filter (\ y -> (third y > third x)) xs))

categoryFilter :: [Package] -> [(String, Float, String)]
categoryFilter ls = map (\ x -> (categoryName x, categoryAllMoney x, categoryMaxMoney x)) (commonCategory ls) 

categoryName :: [Package] -> String
categoryName [] = ""
categoryName l = second (head l)
