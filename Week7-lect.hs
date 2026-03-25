-- LECTURE 7 - ALGEBRAIC TYPES

-- Types we have seen so far ...
--   basic (Int, ...)
--   lists
--   chars, strings
--   tuples
--   type synonyms - using the type keyword; e.g. type Point = (Float, Float)

-- Algebraic types allow us to define completely new types.

-- A Degree type with 5 values (constructors)
data Degree = Pass | Third | LowerSecond | UpperSecond | First 
              deriving (Eq, Ord, Show)

-- the deriving clause allows values to be displayed and compared

-- Note that Bool could be defined by:
-- data Bool = True | False 

-- a function using pattern matching
isGood :: Degree -> Bool
isGood UpperSecond = True
isGood First = True
isGood _ = False

-- an alternative definition based on the ordering of the values
isGood' :: Degree -> Bool
isGood' d = d >= UpperSecond

-- A type where with a single constructor which takes two parameters.
-- Note that constructors can have the same same as the type itself
data Person = Person String Int    -- alternative to    type Person = (String, Int)
        deriving (Eq, Show)

-- projection functions
name :: Person -> String
name (Person n _) = n

age :: Person -> Int
age (Person _ a) = a 

-- test data
people :: [Person]
people = [Person "Fred" 34, Person "Sam" 61, Person "Amy" 27]

-- A list comprehension to give just the names
names :: [Person] -> [String]
names p = [ n | Person n _  <- p ]

-- An alternative using the higher-order function map (and partial evaluation)
names' :: [Person] -> [String]
names' = map name

-- A type with two constructors (with their own parameters)
data Shape = Circle Float | Rectangle Float Float
    deriving (Show, Eq)

-- test data
circle = Circle 10
rectangle = Rectangle 5 4

-- A function using pattern matching
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r 
perimeter (Rectangle h w) = 2 * (h + w)

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



