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


