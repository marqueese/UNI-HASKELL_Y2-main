{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Time
import Data.Maybe

--------------------------------------------------------------------------------
-- DATA TYPE
--------------------------------------------------------------------------------

-- Diary entry: title, content, reviewed?, date

data DiaryEntry = DiaryEntry
    String
    String
    Bool
    Day
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- SAMPLE DATA
--------------------------------------------------------------------------------

diaries :: [DiaryEntry]
diaries =
    [ DiaryEntry "Learnt about GitHub"
                 "I completed a tutorial on how to use GitHub"
                 True
                 (fromGregorian 2023 3 15)

    , DiaryEntry "Met with the clients"
                 "I got to meet with the clients of the project"
                 True
                 (fromGregorian 2023 3 17)

    , DiaryEntry "Completed health and safety training"
                 "I just completed the health and safety training"
                 False
                 (fromGregorian 2023 3 21)

    , DiaryEntry "First staff meeting"
                 "I attended my first staff meeting"
                 True
                 (fromGregorian 2023 3 23)

    , DiaryEntry "Learnt about C"
                 "Did some C stuff"
                 False
                 (fromGregorian 2023 3 24)
    ]

--------------------------------------------------------------------------------
-- ACCESSORS
--------------------------------------------------------------------------------

getTitle :: DiaryEntry -> String
getTitle (DiaryEntry t _ _ _) = t

getContent :: DiaryEntry -> String
getContent (DiaryEntry _ c _ _) = c

getReviewed :: DiaryEntry -> Bool
getReviewed (DiaryEntry _ _ r _) = r

getDate :: DiaryEntry -> Day
getDate (DiaryEntry _ _ _ d) = d

--------------------------------------------------------------------------------
-- FORMATTING
--------------------------------------------------------------------------------

formatEntry :: DiaryEntry -> String
formatEntry (DiaryEntry title content reviewed date) =
    title ++ "\n"
    ++ content ++ "\n"
    ++ (if reviewed then "Reviewed" else "Not reviewed")
    ++ ", " ++ show date ++ "\n"

formatEntries :: [DiaryEntry] -> String
formatEntries [] = ""
formatEntries (x:xs) = formatEntry x ++ "\n" ++ formatEntries xs

--------------------------------------------------------------------------------
-- SEARCH (Maybe)
--------------------------------------------------------------------------------

-- Example input:
-- findEntry "Learnt about GitHub" (fromGregorian 2023 3 15) diaries

findEntry :: String -> Day -> [DiaryEntry] -> Maybe DiaryEntry
findEntry _ _ [] = Nothing
findEntry t d (x:xs)
    | getTitle x == t && getDate x == d = Just x
    | otherwise = findEntry t d xs

--------------------------------------------------------------------------------
-- UPDATE FUNCTIONS
--------------------------------------------------------------------------------

-- Example input:
-- markReviewed "Learnt about C" (fromGregorian 2023 3 24) diaries

markReviewed :: String -> Day -> [DiaryEntry] -> [DiaryEntry]
markReviewed _ _ [] = []
markReviewed t d (x:xs)
    | getTitle x == t && getDate x == d =
        DiaryEntry (getTitle x)
                   (getContent x)
                   True
                   (getDate x)
        : xs
    | otherwise = x : markReviewed t d xs

-- Example input:
-- updateContent "Learnt about C" (fromGregorian 2023 3 24) "New content here" diaries

updateContent :: String -> Day -> String -> [DiaryEntry] -> [DiaryEntry]
updateContent _ _ _ [] = []
updateContent t d newC (x:xs)
    | getTitle x == t && getDate x == d =
        DiaryEntry (getTitle x)
                   newC
                   (getReviewed x)
                   (getDate x)
        : xs
    | otherwise = x : updateContent t d newC xs

--------------------------------------------------------------------------------
-- SHORT ENTRIES
--------------------------------------------------------------------------------

-- Example input:
-- shortEntries diaries

wordCount :: String -> Int
wordCount s = length (words s)

shortEntries :: [DiaryEntry] -> [DiaryEntry]
shortEntries [] = []
shortEntries (x:xs)
    | wordCount (getContent x) < 20 = x : shortEntries xs
    | otherwise = shortEntries xs

--------------------------------------------------------------------------------
-- MAIN MENU
--------------------------------------------------------------------------------

main :: IO ()
main = menu diaries

menu :: [DiaryEntry] -> IO ()
menu ds = do
    putStrLn "\n--- Diary Menu ---"
    putStrLn "1. Mark entry as reviewed"
    putStrLn "   Input: title + date (YYYY-MM-DD)"
    putStrLn "2. Update diary entry"
    putStrLn "   Input: title + date + new content"
    putStrLn "3. Add new entry"
    putStrLn "   Input: title + content + date"
    putStrLn "4. Show short entries"
    putStrLn "   (no input needed)"
    putStrLn "5. Exit"

    option <- getLine

    case option of

        "1" -> do
            putStrLn "Enter title:"
            t <- getLine
            putStrLn "Enter date (use format: fromGregorian YYYY MM DD):"
            d <- getLine
            let newDs = markReviewed t (read d) ds
            putStrLn (formatEntries newDs)
            menu newDs

        "2" -> do
            putStrLn "Enter title:"
            t <- getLine
            putStrLn "Enter date (fromGregorian format):"
            d <- getLine
            putStrLn "Enter new content:"
            c <- getLine
            let newDs = updateContent t (read d) c ds
            putStrLn (formatEntries newDs)
            menu newDs

        "3" -> do
            putStrLn "Enter title:"
            t <- getLine
            putStrLn "Enter content:"
            c <- getLine
            putStrLn "Enter date (fromGregorian YYYY MM DD):"
            d <- getLine
            let newEntry = DiaryEntry t c False (read d)
            let newDs = newEntry : ds
            putStrLn (formatEntries newDs)
            menu newDs

        "4" -> do
            putStrLn (formatEntries (shortEntries ds))
            menu ds

        _ -> return ()



-- M21274 MATHFUN Functional Programming in-class test - Sample

{-
Exercise 1 - 4 marks
--------------------
Using guards or pattern matching, write a price function which returns the
price of an item on a menu, where "Pizza" costs 8.25, "Pasta" costs 7.95, and 
"Salad" costs 6.55. Your function should return 0.0 for any other parameter 
value (i.e. everything else on the menu is free).
-}

price :: String -> Float
price item
    | item == "Pizza" = 8.25
    | item == "Pasta" = 7.95
    | item == "Salad" = 6.55
    | otherwise = 0


{-
Exercise 2 - 4 marks
--------------------
Using a list comprehension and the describeTemperature function below, write a 
describePlaceTemperatures function that takes a list of place-temperature pairs 
(like the testPlaces list), and returns a list of place-descriptions pairs. 
Running your function on testPlaces should give:
  [("London","Cold"),("Madrid","Hot"),("Paris","Warm"),("Helsinki","Freezing")]
-}

testPlaces :: [(String, Int)]
testPlaces = [("London", 12), ("Madrid", 32), ("Paris", 22), ("Helsinki", -3)]

describeTemperature :: Int -> String
describeTemperature temp 
   | temp >= 30   = "Hot"
   | temp >= 15   = "Warm"
   | temp >= 1    = "Cold"
   | otherwise    = "Freezing"
   
describePlaceTemperatures :: [(String, Int)] -> [(String, String)]
describePlaceTemperatures places = [(name, describeTemperature temp) | (name, temp) <- places]


{-
Exercise 3 - 4 marks
--------------------
Using recursion, write a hotter function that takes a temperature and a list 
of place-temperature pairs (like the testPlaces list) and returns a list of 
places from the list that are hotter than the given temperature.
-}

hotter :: Int -> [(String, Int)] -> [String]
hotter _ [] = []
hotter value ((place, temp):xs)
    | temp >  value = place : hotter value xs
    | otherwise = hotter value xs
 

{-
Exercise 4 - 3 marks
--------------------
The List type definition below represents lists of integers, and the testList 
value is an example of an List whose values are in order. Write an insert 
function that takes an int and a List that is ordered, and inserts the integer 
into the List so that it remains ordered. 
-}

data List = Null | Node Int List
     deriving (Show)

testList :: List
testList = Node 2 (Node 3 (Node 5 (Node 5 (Node 6 Null))))

insert :: Int -> List -> List
insert value Null  = Node value Null 

insert value (Node x xs) 
    | value <= x = Node value(Node x xs)
    | otherwise = Node x (insert value xs)

{- 
Test function. Use this function to test your solutions. You should uncomment 
all lines relating to the exercises you have attempted, but should not change 
anything else. 

To test the functionality of your code we will only run this function so
make sure that:
  * you uncommented all appropriate lines in the function
  * you comment out any incomplete/non-working solutions
-}

test :: IO ()
test = do
   putStrLn "Exercise 1"
   putStrLn $ "  Pizza - " ++ show (price "Pizza")
   putStrLn $ "  Pasta - " ++ show (price "Pasta")
   putStrLn $ "  Salad - " ++ show (price "Salad")
   putStrLn $ "  Fish - " ++ show (price "Fish")

   putStrLn "\nExercise 2"
   putStrLn $ "  " ++ show (describePlaceTemperatures testPlaces)

   putStrLn "\nExercise 3"
   putStrLn $ "  " ++ show (hotter 12 testPlaces)

   putStrLn "\nExercise 4"
   putStrLn $ "  Inserting 1 gives - " ++ show (insert 1 testList)
   putStrLn $ "  Inserting 3 gives - " ++ show (insert 3 testList)
   putStrLn $ "  Inserting 4 gives - " ++ show (insert 4 testList)

   putStrLn "\nExercise 5 (listLength)"
   putStrLn $ "  Length - " ++ show (listLength testList)

   putStrLn "\nExercise 6 (warmPlaces)"
   putStrLn $ "  " ++ show (warmPlaces testPlaces)

   putStrLn "\nExtra tests"

   putStrLn "Grade tests"
   putStrLn $ "  35 -> " ++ grade 35
   putStrLn $ "  55 -> " ++ grade 55
   putStrLn $ "  65 -> " ++ grade 65
   putStrLn $ "  85 -> " ++ grade 85

   putStrLn "\nlongWords"
   putStrLn $ "  " ++ show (longWords ["cat", "elephant", "banana", "treehouse"])

   putStrLn "\ncountPositive"
   putStrLn $ "  " ++ show (countPositive [1, -2, 3, 0, 5])

   putStrLn "\ncoldPlaces"
   putStrLn $ "  " ++ show (coldPlaces 10 testPlaces)

   putStrLn "\nhighestTemp"
   putStrLn $ "  " ++ show (highestTemp testPlaces)


--grade test 
grade :: Int -> String
grade value
    | value < 40 = "Fail"
    | value < 60 = "Pass"
    | value < 70 = "Merit"
    | otherwise = "Distinction"


longWords :: [String] -> [String]
longWords [] = []
longWords (word:xs)
    | length word > 5 = word : longWords xs
    | otherwise = longWords xs


countPositive :: [Int] -> Int
countPositive [] = 0
countPositive (x:xs)
    | x > 0 = 1 + countPositive xs
    | otherwise = countPositive xs


coldPlaces :: Int -> [(String, Int)] -> [String]
coldPlaces _ [] = []
coldPlaces value ((place, temp):xs)
    | temp < value = place : coldPlaces value xs  
    | otherwise = coldPlaces value xs


listLength :: List -> Int
listLength Null = 0
listLength (Node _ xs) = 1 + listLength xs


removeValue :: Int -> List -> List
removeValue _ Null = Null
removeValue value (Node x xs)
    | value == x = removeValue value xs
    | otherwise  = Node x (removeValue value xs)


highestTemp :: [(String, Int)] -> Int
highestTemp [(_, temp)] = temp
highestTemp ((_, temp):xs)
    | temp > highestTemp xs = temp
    | otherwise = highestTemp xs

warmPlaces :: [(String, Int)] -> [String]
warmPlaces [] = []

warmPlaces ((place, temp):xs)
    | describeTemperature temp == "Warm"
        = place : warmPlaces xs

    | describeTemperature temp == "Hot"
        = place : warmPlaces xs

    | otherwise
        = warmPlaces xs