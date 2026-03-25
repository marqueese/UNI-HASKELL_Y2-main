data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving(Show, Eq)

data Season = Spring | Summer | Autum  | Winter 
    deriving(Show, Eq)

season_func :: Month -> Season
season_func December = Winter 
season_func January = Winter
season_func February = Winter
season_func March = Spring
season_func April = Spring
season_func May = Spring
season_func June = Summer
season_func July = Summer
season_func August = Summer
season_func September = Autum
season_func October = Autum
season_func November = Autum 

numberOfDays :: Month -> Int -> Int
numberOfDays February year 
    | isLeapYear year = 29
    | otherwise = 28

numberOfDays December _  = 31
numberOfDays January _ = 31
numberOfDays March _ = 31
numberOfDays April _ = 30
numberOfDays May _ = 31
numberOfDays June _ = 30
numberOfDays July _ = 31
numberOfDays August _ = 31
numberOfDays September _ = 30 
numberOfDays October _ = 31
numberOfDays November _ = 30


isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod`  100/= 0) || (year `mod` 400 == 0)

data Point = Point Double Double
    deriving (Show, Eq) --allows comparison

data Shape = Circle Double | Rectangle Double Double
    deriving(Show, Eq) --reads it back to yiu 

data PositionedShape = PositionedShape Shape Point 
    deriving(Show, Eq) --determines ture and false

move :: PositionedShape -> Double -> Double -> PositionedShape
move (PositionedShape shape (Point x y)) dx dy =  --add points x and y to dx,dy
    PositionedShape shape (Point (x + dx) (y + dy)) --x + dx , y + dy to get updated position

--let s = Circle 5
--let p = Point 2 3
--let ps = PositionedShape s p
--move ps 10 4


-- Binary trees
-- A recursive definition:
-- A binary tree is either:
--    A null node
--    A node with an integer value, a left sub tree and a right sub tree

data Tree = Null | Node Int Tree Tree
    deriving (Eq, Show)

-- test data
tree0 = Null
tree1 = Node 3 Null Null
tree2 = Node 5 (Node 3 Null Null) (Node 7 (Node 6 Null Null) Null)

-- An example function using pattern matching and recursion
height :: Tree -> Int
height Null = 0
height (Node _ left right) =  1 +  max (height left) (height right)  

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ left right) = 1 + numberOfNodes left + numberOfNodes right

isMember :: Int -> Tree -> Bool 
isMember _ Null = False  -- tree is empty is false 
isMember x (Node value left right) -- node value then left or right 
    | x == value = True  -- if x is in tree is true 
    | otherwise = isMember x left || isMember x right  -- if it exists in either retuen true 

leaves :: Tree -> [Int]
leaves Null = [] --empty
leaves (Node value Null Null) = [value] -- individual branch is a leaf 
leaves (Node _ left right) = leaves left ++ leaves right -- add together 

inOrder :: Tree -> [Int]
inOrder Null = [] --empty
inOrder (Node value left right) = inOrder left ++ [value] ++inOrder right  -- add together agfin 
--go  left, value , right 

insert :: Int -> Tree -> Tree 
insert x Null = Node x Null Null -- empty tree add 
insert x (Node value left right) 
    | x < value = Node value (insert x left ) right -- smaller value 
    | x > value = Node value left (insert x right)  
    | otherwise = Node value left right --nodes are the same 

listToSearchTree :: [Int] -> Tree
listToSearchTree [] = Null
listToSearchTree (x:xs) = --go through list addind
    insert x (listToSearchTree xs)

binaryTreeSort :: [Int] -> [Int]
binaryTreeSort xs = inOrder(listToSearchTree xs)