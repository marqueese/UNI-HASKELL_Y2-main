headPlusOne :: [Int] -> Int
headPlusOne [] =  -1
headPlusOne (x:xs) = x + 1 -- x = item[0], xs = item[1:]

duplicateHead :: [Char] -> [Char]
duplicateHead [] = []
duplicateHead (x:xs) = x:x:xs

rotate :: [a] -> [a]
rotate [] = []
rotate (x:y:xs) = y:x:xs -- 1,2,3 - 2,1,3

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs -- length of list 

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs -- get this its crazy, it multiplies 

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs -- get this its crazy, it ands all the bools together

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs

countIntegers :: Int -> [Int] -> Int
countIntegers n [] = 0
countIntegers n (x:xs) = if n == x then 1 + countIntegers n xs else countIntegers n xs -- if n == x its a match so add 1 to the count, if not just keep counting

removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs) = if n == x then removeAll n xs else x: removeAll n xs -- if n == x its a match so remove it, if not keep it in the list and keep checking the rest of the list

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst n [] = []
removeAllButFirst n (x:xs)
    | n == x = x : [y | y <- xs, y /= n] -- if n == x its a match and first keep it, in the brackets return everthing tht is not n 
    | otherwise = x : removeAllButFirst n xs

type StudentMark = (String, Int) 

testData :: [StudentMark] 
testData = 
    [ 
    ("John", 53), 
    ("Sam", 16), 
    ("Kate", 85), 
    ("Jill", 65), 
    ("Bill", 37), 
    ("Amy", 22), 
    ("Jack", 41), 
    ("Sue", 71) 
    ]

listMarks :: String -> [StudentMark] ->[Int]
listMarks student marks = [mark | (name, mark) <- marks , name == student] 

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs) -- if x is less than y and the rest of the list is sorted then the whole list is sorted, if not then its not sorted

prefix :: [Int] -> [Int] -> Bool 
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys -- if 1 ==1 && 2 == 2  then its a prefix, if not then its not a prefix

