-------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	Chapter 5
--
-------------------------------------------------------------------------

module Chapter5 where

import Prelude hiding (id)
import Test.QuickCheck
import Data.Char 

-- Data types: tuples and lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Introducing tuples, lists and strings
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

type ShopItem = (String,Int)
type Basket   = [ShopItem]

basket1 :: Basket
basket1 = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Gin: 1lt",1099) ]

basket2 :: Basket
basket2 = []

basket3 :: Basket
basket3 = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Plain crisps",25) ]


-- Tuple types
-- ^^^^^^^^^^^

-- Minimum and maximum of two integers.

minAndMax :: Integer -> Integer -> (Integer,Integer)
minAndMax x y
  | x>=y        = (y,x)
  | otherwise   = (x,y)

-- Adding a pair of intgers.

addPair :: (Integer,Integer) -> Integer
addPair (x,y) = x+y

-- Shifting around the structure of an ((Int,Int),Int).

shift :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shift ((x,y),z) = (x,(y,z))

-- Selecting parts of a tuple

name  :: ShopItem -> String
price :: ShopItem -> Int

name  (n,p) = n
price (n,p) = p

-- Adding a pair using the built-in selectors, fst and snd.

addPair' :: (Integer,Integer) -> Integer
addPair' p = fst p + snd p

-- Fibonacci numbers: an efficient function, fastFib.

fibStep :: (Integer,Integer) -> (Integer,Integer)
fibStep (u,v) = (v,u+v)

fibPair :: Integer -> (Integer,Integer)
fibPair n
  | n==0        = (0,1)
  | otherwise   = fibStep (fibPair (n-1))

fastFib :: Integer -> Integer
fastFib = fst . fibPair

fibTwoStep :: Integer -> Integer -> (Integer,Integer)
fibTwoStep x y = (y,x+y)

-- Introducing algebraic types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- We give a sequence of examples of increasing complexity ...


-- Product types
-- ^^^^^^^^^^^^^

-- A person is represented by their name and age ...

data People = Person Name Age

-- where Name and Age are the appropriate synonyms.

type Name = String
type Age  = Int

jemima, ronnie :: People
jemima = Person "Electric Aunt Jemima" 77
ronnie = Person "Ronnie" 14

-- Turning a person into a string.

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n

-- An alternative to Age,

data NewAge = Years Int


-- Alternatives
-- ^^^^^^^^^^^^

-- A shape in a simple geometrical program is either a circle or a
-- rectangle. These alternatives are given by the type

data Shape = Circle Float |
             Rectangle Float Float
	     deriving (Eq,Ord,Show,Read)

shape1 = Circle 3.0
shape2 = Rectangle 45.9 87.6

-- Pattern matching allows us to define functions by cases, as in,

isRound :: Shape -> Bool
isRound (Circle _)      = True
isRound (Rectangle _ _) = False

-- and also lets us use the components of the elements:

area :: Shape -> Float
area (Circle r)      = pi*r*r
area (Rectangle h w) = h*w

-- Derived instances ...

--	data Season = Spring | Summer | Autumn | Winter 
--	              deriving (Eq,Ord,Enum,Show,Read)



-- Lists in Haskell
-- ^^^^^^^^^^^^^^^^

-- Various examples of lists

list1 :: [Integer]
list1 = [1,2,3,4,1,4]

list2 :: [Bool]
list2 = [True]

list3 :: String
list3 = ['a','a','b']

list4 :: String
list4 = "aab"

list5 :: [ Integer -> Integer ]
list5 = [fastFib,fastFib]

list6  :: [ [Integer] ]
list6 = [[12,2],[2,12],[]]

list7 :: [Integer]
list7 = [2 .. 7]

list8 :: [Float]
list8 = [3.1 .. 7.0]

list9 :: String
list9 = ['a' .. 'm']

list10 :: [Integer]
list10 = [7,6 .. 3]

list11 :: [Float]
list11 = [0.0,0.3 .. 1.0]

list12 :: String
list12 = ['a','c' .. 'n']


-- List comprehensions
-- ^^^^^^^^^^^^^^^^^^^
-- Examples of list comprehensions

ex :: [Integer]
ex = [2,4,7]

comp1 :: [Integer]
comp1 = [ 2*n | n<-ex]

comp2 :: [Bool]
comp2 = [ isEven n | n<-ex ]

isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)

comp3 :: [Integer]
comp3 = [ 2*n | n <- ex , isEven n , n>3 ]

-- Add all the pairs in a list of pairs.

addPairs :: [(Integer,Integer)] -> [Integer] 
addPairs pairList = [ m+n | (m,n) <- pairList ]

-- Return only the sums of pairs which are increasing.

addOrdPairs :: [(Integer,Integer)] -> [Integer]
addOrdPairs pairList = [ m+n | (m,n) <- pairList , m<n ]

-- Return only the digits in a String.

digits :: String -> String
digits st = [ ch | ch<-st , isDigit ch ] 

-- Are all the integers in a list even? or odd?

allEven, allOdd :: [Integer] -> Bool
allEven xs = (xs == [x | x<-xs, isEven x])
allOdd xs  = ([] == [x | x<-xs, isEven x])

-- Summing the radii of the circles in a list, ignores the other shapes

totalRadii :: [Shape] -> Float
totalRadii shapes = sum [r | Circle r <- shapes]

-- Extracting all the singletons in a list of integer lists, 
-- ignoring the other lists.

sings :: [[Integer]] -> [Integer]
sings xss = [x | [x] <-xss ]


-- A library database
-- ^^^^^^^^^^^^^^^^^^

-- Types

type Person = String
type Book   = String

type Database = [ (Person , Book) ]

-- An example database.

exampleBase :: Database
exampleBase 
  = [ ("Alice" , "Tintin")  , ("Anna" , "Little Women") ,
      ("Alice" , "Asterix") , ("Rory" , "Tintin") ]

-- The books borrowed by a particular person in the given database.

books       :: Database -> Person -> [Book]
books dBase findPerson
  = [ book | (person,book) <- dBase , person==findPerson ]

-- Making a loan is done by adding a pair to the database.

makeLoan   :: Database -> Person -> Book -> Database
makeLoan dBase pers bk = [ (pers,bk) ] ++ dBase

-- To return a loan.

returnLoan   :: Database -> Person -> Book -> Database
returnLoan dBase pers bk
  = [ pair | pair <- dBase , pair /= (pers,bk) ]

-- Testing the database.

-- Commented out because borrowed is not defined here.

-- test1 :: Bool
-- test1 = borrowed exampleBase "Asterix"

test2 :: Database
test2 = makeLoan exampleBase "Alice" "Rotten Romans"

-- QuickCheck properties for the database

-- Check that bk is in the list of loaned books to pers
-- after making the loan of book to pers

prop_db1 :: Database -> Person -> Book -> Bool

prop_db1 dBase pers bk =
    elem bk loanedAfterLoan == True
         where
           afterLoan = makeLoan dBase pers bk
           loanedAfterLoan = books afterLoan pers

-- Check that bk is not in the list of loaned books to pers
-- after returning the loan of book to pers

prop_db2 :: Database -> Person -> Book -> Bool

prop_db2 dBase pers bk =
    elem bk loanedAfterReturn == False
         where
           afterReturn = returnLoan dBase pers bk
           loanedAfterReturn = books afterReturn pers


