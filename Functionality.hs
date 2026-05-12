import Data.Char

--------------------------------------------------------------------------------
-- HASKELL CHEAT SHEET & REFERENCE FILE
--------------------------------------------------------------------------------
-- This file contains:
--   * Basic function syntax
--   * Guards
--   * Pattern matching
--   * Recursion
--   * List comprehensions
--   * Tuples
--   * Type synonyms
--   * Algebraic data types
--   * Useful built-in functions
--   * Larger example functions
--   * IO Testing
--
-- Then test functions like:
--      square 4
--      factorial 5
--      reverseList [1,2,3]
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- 1. BASIC FUNCTION SYNTAX
--------------------------------------------------------------------------------

-- Simple function
square :: Int -> Int
square n = n * n

-- Example:
-- square 4
-- Result: 16


-- Multiple parameters
sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

-- Example:
-- sumThree 2 3 4
-- Result: 9


--------------------------------------------------------------------------------
-- 2. BOOLEAN FUNCTIONS
--------------------------------------------------------------------------------

canDrink :: Int -> Bool
canDrink age = age >= 18

-- Example:
-- canDrink 20
-- True

-- canDrink 15
-- False


--------------------------------------------------------------------------------
-- 3. LOGICAL OPERATORS
--------------------------------------------------------------------------------

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c =
    canDrink a && canDrink b && canDrink c

-- Example:
-- all3CanDrink 20 21 18
-- True


--------------------------------------------------------------------------------
-- 4. GUARDS
--------------------------------------------------------------------------------

absolute :: Int -> Int
absolute x
    | x < 0 = 0 - x
    | otherwise = x

-- Example:
-- absolute (-5)
-- 5


signValue :: Int -> Int
signValue x
    | x < 0 = -1
    | x > 0 = 1
    | otherwise = 0


--------------------------------------------------------------------------------
-- 5. IF STATEMENTS
--------------------------------------------------------------------------------

countIntegers :: Int -> [Int] -> Int
countIntegers n [] = 0
countIntegers n (x:xs) =
    if n == x
    then 1 + countIntegers n xs
    else countIntegers n xs

-- Example:
-- countIntegers 3 [1,3,3,4,5]
-- 2


--------------------------------------------------------------------------------
-- 6. PATTERN MATCHING
--------------------------------------------------------------------------------

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

-- Example:
-- headPlusOne [5,6,7]
-- 6


--------------------------------------------------------------------------------
-- 7. RECURSION
--------------------------------------------------------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Example:
-- factorial 5
-- 120


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n =
    fibonacci (n - 1) + fibonacci (n - 2)

-- Example:
-- fibonacci 7
-- 13


--------------------------------------------------------------------------------
-- 8. LIST COMPREHENSIONS
--------------------------------------------------------------------------------

firstSquare :: Int -> [Int]
firstSquare n = [x^2 | x <- [1..n]]

-- Example:
-- firstSquare 5
-- [1,4,9,16,25]


doublePositives :: [Int] -> [Int]
doublePositives xs =
    [x * 2 | x <- xs, x > 0]

-- Example:
-- doublePositives [1,-3,-2,4,1,9]
-- [2,8,2,18]


--------------------------------------------------------------------------------
-- 9. TUPLES
--------------------------------------------------------------------------------

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

-- Example:
-- sumDifference 10 3
-- (13,7)


--------------------------------------------------------------------------------
-- 10. TYPE SYNONYMS
--------------------------------------------------------------------------------

type StudentMark = (String, Int)

student1 :: StudentMark
student1 = ("Marcus", 85)


--------------------------------------------------------------------------------
-- 11. STRINGS
--------------------------------------------------------------------------------

-- Remember:
-- String == [Char]

capitalise :: String -> String
capitalise xs = [toUpper x | x <- xs]

-- Example:
-- capitalise "hello"
-- "HELLO"


onlyDigits :: String -> String
onlyDigits xs = [x | x <- xs, isDigit x]

-- Example:
-- onlyDigits "abc123def456"
-- "123456"


--------------------------------------------------------------------------------
-- 12. BUILT-IN FUNCTIONS
--------------------------------------------------------------------------------

circleArea :: Float -> Float
circleArea r = pi * r^2


squareRootExample :: Float -> Float
squareRootExample x = sqrt x


modExample :: Int -> Int
modExample x = mod x 2


--------------------------------------------------------------------------------
-- 13. LISTS
--------------------------------------------------------------------------------

-- Adding item to front of list
exampleList :: [Int]
exampleList = 1 : [2,3,4]

-- Result:
-- [1,2,3,4]


--------------------------------------------------------------------------------
-- 14. GENERIC TYPES
--------------------------------------------------------------------------------

reverseGeneric :: [a] -> [a]
reverseGeneric [] = []
reverseGeneric (x:xs) = reverseGeneric xs ++ [x]


--------------------------------------------------------------------------------
-- 15. ALGEBRAIC DATA TYPES
--------------------------------------------------------------------------------

data Season = Spring | Summer | Autumn | Winter
    deriving (Show, Eq)


data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Show, Eq)


seasonFunc :: Month -> Season
seasonFunc December = Winter
seasonFunc January = Winter
seasonFunc February = Winter
seasonFunc March = Spring
seasonFunc April = Spring
seasonFunc May = Spring
seasonFunc June = Summer
seasonFunc July = Summer
seasonFunc August = Summer
seasonFunc September = Autumn
seasonFunc October = Autumn
seasonFunc November = Autumn


--------------------------------------------------------------------------------
-- 16. IMPORTANT RECURSION PATTERNS
--------------------------------------------------------------------------------

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs


multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs


andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs


removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x : removeAll n xs


--------------------------------------------------------------------------------
-- 17. COMBINING MULTIPLE CONCEPTS
--------------------------------------------------------------------------------

-- Uses recursion + guards + lists
squareEvens :: [Int] -> [Int]
squareEvens [] = []
squareEvens (x:xs)
    | even x = x^2 : squareEvens xs
    | otherwise = squareEvens xs

-- Example:
-- squareEvens [1,2,3,4,5,6]
-- [4,16,36]


-- Count uppercase letters
countUppercase :: String -> Int
countUppercase [] = 0
countUppercase (x:xs)
    | isUpper x = 1 + countUppercase xs
    | otherwise = countUppercase xs

-- Example:
-- countUppercase "HeLLo"
-- 3


-- Average of list
averageList :: [Int] -> Float
averageList xs =
    fromIntegral (sumList xs) /
    fromIntegral (length xs)

-- Example:
-- averageList [10,20,30]
-- 20.0


-- Reverse list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Example:
-- reverseList [1,2,3,4]
-- [4,3,2,1]


-- Palindrome checker
isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

-- Example:
-- isPalindrome "level"
-- True


--------------------------------------------------------------------------------
-- 18. EXTRA PRACTICE FUNCTIONS
--------------------------------------------------------------------------------

-- Keep only odd numbers and double them
oddDouble :: [Int] -> [Int]
oddDouble [] = []
oddDouble (x:xs)
    | odd x = (x * 2) : oddDouble xs
    | otherwise = oddDouble xs

-- Example:
-- oddDouble [1,2,3,4,5]
-- [2,6,10]


-- Count vowels
countVowels :: String -> Int
countVowels [] = 0
countVowels (x:xs)
    | x `elem` "aeiouAEIOU" =
        1 + countVowels xs
    | otherwise =
        countVowels xs

-- Example:
-- countVowels "Programming"
-- 3


-- Largest number in list
largest :: [Int] -> Int
largest [x] = x
largest (x:xs)
    | x > largest xs = x
    | otherwise = largest xs

-- Example:
-- largest [5,2,10,3]
-- 10


--------------------------------------------------------------------------------
-- 19. HIGHER ORDER FUNCTIONS
--------------------------------------------------------------------------------

-- map applies a function to every item
mapExample :: [Int] -> [Int]
mapExample xs = map square xs

-- Example:
-- mapExample [1,2,3]
-- [1,4,9]


-- filter keeps matching values
filterExample :: [Int] -> [Int]
filterExample xs = filter even xs

-- Example:
-- filterExample [1,2,3,4,5,6]
-- [2,4,6]


--------------------------------------------------------------------------------
-- 20. MORE ADVANCED EXAMPLES
--------------------------------------------------------------------------------

-- Sum only positive numbers
sumPositives :: [Int] -> Int
sumPositives [] = 0
sumPositives (x:xs)
    | x > 0 = x + sumPositives xs
    | otherwise = sumPositives xs


-- Count occurrences of a character
countChar :: Char -> String -> Int
countChar c [] = 0
countChar c (x:xs)
    | c == x = 1 + countChar c xs
    | otherwise = countChar c xs


-- Remove duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs


-- Check if list is sorted
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs)
    | x <= y = isSorted (y:xs)
    | otherwise = False


--------------------------------------------------------------------------------
-- 21. HELPER FUNCTIONS
--------------------------------------------------------------------------------

-- Helper function example
power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n - 1)


-- Helper function used internally
sumSquares :: [Int] -> Int
sumSquares xs = helper xs 0
    where
        helper [] total = total
        helper (x:xs) total =
            helper xs (total + x^2)


--------------------------------------------------------------------------------
-- 22. QUICK TESTS
--------------------------------------------------------------------------------

-- Try these in GHCi:
--
-- square 9
-- factorial 6
-- fibonacci 10
-- firstSquare 8
-- reverseList [1,2,3,4]
-- isPalindrome "racecar"
-- countVowels "Hello World"
-- largest [3,7,2,9,1]
-- averageList [5,10,15]
-- removeDuplicates [1,2,2,3,3,3,4]
-- isSorted [1,2,3,4]
-- isSorted [1,5,3,4]


--------------------------------------------------------------------------------
-- 23. INPUT / OUTPUT (I/O)
--------------------------------------------------------------------------------
-- IMPORTANT:
-- Most Haskell functions are PURE.
-- Pure functions:
--      same input -> same output
--
-- I/O functions interact with:
--      * keyboard
--      * files
--      * terminal
--
-- I/O functions use the IO type.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- SIMPLE OUTPUT
--------------------------------------------------------------------------------

hello :: IO ()
hello = putStrLn "Hello World!"

-- Run in GHCi:
-- hello


--------------------------------------------------------------------------------
-- SIMPLE INPUT
--------------------------------------------------------------------------------

askName :: IO ()
askName = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name)

-- Example:
-- What is your name?
-- Marcus
-- Hello Marcus


--------------------------------------------------------------------------------
-- MULTIPLE INPUTS
--------------------------------------------------------------------------------

addTwoNumbers :: IO ()
addTwoNumbers = do
    putStrLn "Enter first number:"
    num1 <- getLine

    putStrLn "Enter second number:"
    num2 <- getLine

    let x = read num1 :: Int
    let y = read num2 :: Int

    putStrLn ("Total = " ++ show (x + y))

-- IMPORTANT:
-- read converts String -> Int
-- show converts Int -> String


--------------------------------------------------------------------------------
-- USING PURE FUNCTIONS INSIDE I/O
--------------------------------------------------------------------------------

squareIO :: IO ()
squareIO = do
    putStrLn "Enter a number:"
    num <- getLine

    let x = read num :: Int

    putStrLn ("Square = " ++ show (square x))

-- Uses previously defined square function


--------------------------------------------------------------------------------
-- SIMPLE MENU SYSTEM
--------------------------------------------------------------------------------

menu :: IO ()
menu = do
    putStrLn "1. Say hello"
    putStrLn "2. Square a number"
    putStrLn "3. Exit"

    choice <- getLine

    if choice == "1"
    then putStrLn "Hello!"
    else if choice == "2"
    then squareIO
    else putStrLn "Goodbye"


--------------------------------------------------------------------------------
-- RECURSIVE I/O LOOP
--------------------------------------------------------------------------------

counterLoop :: Int -> IO ()
counterLoop n = do
    putStrLn ("Counter = " ++ show n)

    if n <= 0
    then putStrLn "Finished!"
    else counterLoop (n - 1)

-- Example:
-- counterLoop 5


--------------------------------------------------------------------------------
-- FILE WRITING
--------------------------------------------------------------------------------

writeExample :: IO ()
writeExample = do
    writeFile "example.txt" "Hello from Haskell!"
    putStrLn "File written."

-- Creates:
-- example.txt


--------------------------------------------------------------------------------
-- FILE READING
--------------------------------------------------------------------------------

readExample :: IO ()
readExample = do
    contents <- readFile "example.txt"
    putStrLn contents


--------------------------------------------------------------------------------
-- APPENDING TO FILES
--------------------------------------------------------------------------------

appendExample :: IO ()
appendExample = do
    appendFile "example.txt" "\nNew line added."
    putStrLn "Text appended."


--------------------------------------------------------------------------------
-- FOREVER LOOP EXAMPLE
--------------------------------------------------------------------------------

echoLoop :: IO ()
echoLoop = do
    putStrLn "Type something:"
    text <- getLine

    putStrLn ("You typed: " ++ text)

    echoLoop

-- WARNING:
-- infinite loop until stopped


--------------------------------------------------------------------------------
-- COMMON I/O FUNCTIONS
--------------------------------------------------------------------------------
--
-- putStrLn   -> print line
-- putStr     -> print without newline
-- getLine    -> read input
-- read       -> convert String to value
-- show       -> convert value to String
-- readFile   -> read file
-- writeFile  -> overwrite file
-- appendFile -> add to file
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- END OF CHEAT SHEET
--------------------------------------------------------------------------------