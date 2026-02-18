absolute :: Int -> Int
absolute x
    | x < 0     = 0 - x
    | otherwise = x

sign :: Int -> Int
sign x
    | x < 0     = (-1)
    | x > 0     = 1
    | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = 
    sqrt (2 * ((a * a) + (b * b) + (c * c)))

taxiFare :: Int -> Float
taxiFare a =
    let km = fromIntegral a
    in if km <= 10
       then 2.2 + km * 0.5
    else 2.2 + 10 * 0.5 + (km - 10) * 0.3


howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c =
    let average  = fromIntegral (a + b + c) / 3
    in length (filter (> average) [fromIntegral a, fromIntegral b, fromIntegral c])

--123, 143 

validDate :: Int -> Int -> Bool
validDate a b 
    | a >= 1 && a<= 31 && b >= 1 && b <= 12 = True
    | otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year 
    | month == 2 && isLeapYear year = 29
    | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = 31
    | month == 4 || month == 6 || month == 9 || month == 11 = 30
    | otherwise = 0

isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod`  100/= 0) || (year `mod` 400 == 0)



{- 
ghci> sumThree 3 5 7
15

ghci> sumThree 8 (1 + 3) 2
14

ghci> threeDifferent 1 4 2 
True

ghci>  threeDifferent 1 7 7
False

ghci> howManyEqual 3 5 2
0

ghci>  howManyEqual 5 2 5
2
-}

