--basic functions--
import Prelude
import Distribution.Compat.Lens (_1)

double x = x * 2

addThree:: Int-> Int-> Int -> Int
addThree a b c = a + b + c 


--Guards --
absolute:: Int -> Int 
absolute x 
    | x < 0 = x - 0
    | otherwise = x


grade:: Int -> String
grade x
    | x >= 70 = "A"
    | x >= 50 = "B"
    | x >= 40 = "C"
    | x >= 30 = "D"
    | x >= 20 = "You suck"

gradeList :: [Int] -> [String]
gradeList [] = []
gradeList (x:xs)
    | x >= 70 = "A" : gradeList xs
    | x >= 50 = "B" : gradeList xs
    | x >= 40 = "C" : gradeList xs
    | x >= 30 = "D" : gradeList xs
    | otherwise = "You suck" : gradeList xs

--Pattern Matching--

isZero:: Int -> Bool
isZero x 
    | x < 0 || x > 0 = False
    | otherwise = True

headOfList:: [a] -> Maybe a  --potentially an element
headOfList [] = Nothing --nothing there
headOfList (x:_) = Just x --just x


factorial :: Int -> Int
factorial 0 = 1 --initialise 1 
factorial n = n * factorial (n - 1) -- 5 * 4 * 3 * 2 * 1
func_1 :: IO ()
func_1 = do
   let num = 5
   print (factorial num) 


sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs --from x (1) + (2,3,4,5,)

sumList1 :: [Int] -> Int
sumList1 xs = foldl (+) 0 xs --add values starting from the left

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]


listLength :: [a] -> Int
listLength [] = 0                        --list length starts at 0 
listLength (x:xs) = 1 + listLength xs --1 + every list item until xs (the end) is reached



squaredNumbers:: [Int] -> [Int] 
squaredNumbers [] = []
squaredNumbers (x:xs) = x^2 : squaredNumbers xs

first (a,b) = a --first element of a tuple 

first_1 :: (a, b) -> a
first_1 (a, _) = a

t = (5, "hi")
mySwap (a,b) = (b,a)

func_2 :: IO ()
func_2 = do
    print (fst t)
    print (mySwap t) --reverse ab to ba 

data Shape = Circle Float | Rectangle Float Float  | Triangle Float Float --function anme and data types

area :: Shape -> Float
area (Circle r) = 3.14159 * r * r --formulas
area (Rectangle w h) = w * h
area (Triangle b h) = 0.5 * b * h

 --area(Triangle 6 2)

countPositives :: [Int] -> [Int]
countPositives [] = []
countPositives (x:xs)
    | x > 0 = x : countPositives xs -- if x > 0 = x, next number 
    | otherwise = countPositives xs -- doesnt meet condition one, next

doubleEvens :: [Int] -> [Int]
doubleEvens [] = []
doubleEvens (x:xs)
    | even x = (2 * x) : doubleEvens xs
    | otherwise = x : doubleEvens xs

doubleOdds :: [Int] -> [Int]
doubleOdds [] = []
doubleOdds (x:xs)
    | odd x = (2 * x) : doubleOdds xs
    | otherwise = x : doubleOdds xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)