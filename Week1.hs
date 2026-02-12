square :: Int -> Int
square n = n * n

mult2 :: Int -> Int
mult2 x = 2 * x

mult4 :: Int -> Int
mult4 x = 4 * x

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen x = 10 * x 

sumThree :: Int -> Int -> Int -> Int
sumThree a b c  = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle x = pi * x * x

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder x y = areaOfCircle y * x

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

isEven :: Int -> Bool
isEven x = divisibleBy x 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral(a + b + c) / 3
--fromIntegral makes it work by reading them as ints--

absolute :: Int -> Int
absolute x = if x < 0 then 0 - x else x

