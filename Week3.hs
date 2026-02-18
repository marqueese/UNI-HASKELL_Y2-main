{-
import Prelude hiding ((||),(&&))

infixr 3 &&

-- Definition 1: Both arguments pattern
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

-- Definition 2: First argument pattern
(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

-- Definition 3: Second argument pattern
(&&) :: Bool -> Bool -> Bool
b && True = b
_ && False = False

-- Similarly for || (you need to write these too)
infixr 2 ||  -- Fixity for ||

-- Definition 1 for ||
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

-- Definition 2 for ||
(||) :: Bool -> Bool -> Bool
False || b = b
True || _ = True

-- Definition 3 for ||
(||) :: Bool -> Bool -> Bool
b || False = b
_ || True = True
-}

nor :: Bool-> Bool-> Bool
nor False False = True
nor False True = False
nor True False = False  
nor True True = False

fibonacci :: Int -> Int 
fibonacci a 
    | a == 0 = 0
    | a == 1 = 1
    | otherwise = fibonacci (a - 1) + fibonacci (a - 2)

exOr :: Bool-> Bool-> Bool
exOr False False = False
exOr False True = True
exOr True False = True
exOr True True = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x -- if position one is True retrn Value oen 
ifThenElse False _ y = y -- if position one is False retrn Value two

daysInMonth :: Int-> Int
daysInMonth month 
    | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = 31
    | month == 4 || month == 6 || month == 9 || month == 11 = 30
    | month == 2 = 28
    | otherwise = error "Stop trying to break my code"

sumNumbers :: Int-> Int
sumNumbers n = sum [1..n] -- simple code, pain in the ass though 
--[1..n] = [1,2,3,4,5]

sumSquares :: Int-> Int
sumSquares n = sum [x^2 | x <- [1..n]] 
--[x^2 | x <- [1..n]] = [1^2, 2^2, 3^2] = [1,4,9] = 14 (still an ass )

power :: Int-> Int-> Int
power x y = x^y

sumFromTo :: Int-> Int-> Int
sumFromTo a b 
    | a > b = 0
    | otherwise = a + sumFromTo ( a + 1 ) b

myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)


