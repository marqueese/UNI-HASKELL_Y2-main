pairs :: [Int] -> [Int]
pairs [] = []
pairs [_] = [] 
pairs (x:y:xs) = (x + y) : pairs (y : xs)



--pairs [1 ,2, 3, 4, 5]