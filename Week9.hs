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
