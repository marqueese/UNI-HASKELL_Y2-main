--double checked nowhere in document 4 does it say i cannot use map 
import Data.Char --capitalises stuff 

--sumDifference
sumDifference :: Int-> Int-> (Int,Int)
sumDifference x y = (x + y, x - y)

type StudentMark = (String, Int)

grade :: StudentMark -> (String, Char)
grade (name, mark)
    | mark >= 70 = (name, 'A')
    | mark >= 60 = (name, 'B')
    | mark >= 50 = (name, 'C')
    | mark >= 40 = (name, 'D')
    | otherwise = (name, 'F')

capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark > 40 = (name, 40)
    | otherwise = (name, mark)


firstNumbers :: Int-> [Int]
firstNumbers n = [1..n] ---- 5 = [1,2,3,4,5]

firstSquare :: Int -> [Int]
firstSquare n = [x^2 | x <- firstNumbers n] -- 1^2 for each 1 to n 

--Using a list comprehension, write a function that capitalizes a given string.
capitalise :: String-> String
capitalise xs = [toUpper x | x <- xs] --imported thing to capitalise stuff

onlyDigits :: String-> String
onlyDigits xs = [x | x <- xs, isDigit x] --imported thing to filter out digits from string

capMarks :: [StudentMark] -> [StudentMark]
capMarks marks = [capMark mark | mark <- marks]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents marks = [grade mark | mark <- marks]

duplicate:: String-> Int-> String
duplicate str n = concat (replicate n str) --join strings, replicate suprisingly replocates is n times, str is also the string 

divisors :: Int-> [Int]
divisors n = [x |x <- [1..n], n `mod` x == 0] --for loop returns stuff if the remainder of n divided  by x = 0 

isPrime :: Int-> Bool
isPrime n = length (divisors n) == 2 --call divisors if theres not 2 then it aint prime, if there is, good news its a prime

split:: [(a,b)]-> ([a],[b])
split = unzip --more built in haskell stuff to split a list
--beyond unzup3 then i'd have to write my own stuff 

--split :: [(a,b)] -> ([a],[b])
--split xs = ([a | (a,b) <- xs], [b | (a,b) <- xs]) -- splits lists into a and b with elements a and b being divided by xs and a , 

--split :: [(a,b)] -> ([a],[b])
--split xs = (map fst xs, map snd xs) --fst and snd suprisingly stand for first and second so map it to list a and b 



