mult10 :: [Int] -> [Int]
mult10 [] = []

mult10 (x:xs) = (x * 10) : mult10 xs -- start to end of list, times by 10 and add to new list

onlyLowerCase :: [Char] -> [Char]
onlyLowerCase [] = []
onlyLowerCase (x:xs)
    | x >= 'a' && x <= 'z' = x :onlyLowerCase xs -- if x is ower case keep it, if not bin it
    | otherwise = onlyLowerCase xs -- bin it 

orAll :: [Bool] -> Bool
orAll []     = False
orAll (x:xs) 
    | x         = True --if the first item is true good 
    | otherwise = orAll xs -- keep chemkign ti the list is empty 

sumSquares :: [Int] -> Int
sumSquares [] = 0   
sumSquares (x:xs) = x^2 + sumSquares xs -- square nd add 

zeroToTen :: [Int] -> [Int]
zeroToTen [] = []
zeroToTen (x:xs) 
    | x >= 0 && x <= 10 = x : zeroToTen xs 
    | otherwise = zeroToTen xs

--get swuare roots 
squareRoutes:: [Int] -> [Double]
squareRoutes [] = []
squareRoutes (x:xs) 
    | x >= 0  = sqrt(fromIntegral x) : squareRoutes xs
    | otherwise = squareRoutes xs

countBetween :: Float-> Float-> [Float]-> Int
countBetween low high [] = 0
countBetween low high (x:xs) 
    | x >= low && x <= high = 1 + countBetween low high xs
    | otherwise = countBetween low high xs

alwaysPositive :: (Float-> Float)-> [Float]-> Bool
alwaysPositive f [] = True
alwaysPositive f (x:xs) 
    | f x > 0 = alwaysPositive f xs --if the first is positive keep checking the rest of the list
    | otherwise = False -- if the first is not positive then return false

alwaysPositive2 :: (Float -> Float) -> [Float] -> Bool
alwaysPositive2 f xs = all (> 0) (map f xs)

alwaysPositive3 :: (Float -> Float) -> [Float] -> Bool
alwaysPositive3 f xs = and [f x > 0 | x <- xs]  

productSquareroots :: [Int] -> Double
productSquareroots [] = 1
productSquareroots (x:xs)
    | x >= 0 = sqrt(fromIntegral x) * productSquareroots xs
    | otherwise = productSquareroots xs

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst p (x:xs)
  | p x       = xs          --first to meet condition is removed 
  | otherwise = x : removeFirst p xs 

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast _ [] = []
removeLast p xs = reverse (removeFirst p (reverse xs)) -- reverse list delete stuff and done 


--labdma of question 5 
zeroToTen2 :: [Integer] -> [Integer]
zeroToTen2 = filter (\x -> x >= 0 && x <= 10) --filter just sets conditions for what to keep  


--Write similar definitions for the following functions: 
--(a) alwaysPositive 
--(b) productSquareRoots 
--(c) reverse 

alwaysPositive4 :: (Float -> Float) -> [Float] -> Bool
alwaysPositive4 f = all (> 0) . map f

productSquareroots2 :: [Int] -> Double
productSquareroots2 = foldr (*) 1 . map (sqrt . fromIntegral)

reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

