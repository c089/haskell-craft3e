-------------------------------------------------------------------------
--
--	Haskell: The Craft of Functional Programming, 3e
--	Simon Thompson
--	(c) Addison-Wesley, 1996-2011.
--
--	Chapter 7
--
-------------------------------------------------------------------------

module Chapter7 where

-- Defining functions over lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- For pedagogical reasons, this chapter repeats many of the definitions in the
-- standard Prelude. They are repeated in this file, and so the original
-- definitions have to be hidden when the Prelude is imported:

import Prelude hiding (id,head,tail,null,sum,concat,(++),zip,take,getLine)
import qualified Prelude

import Chapter5 (digits,isEven) 
import Test.QuickCheck

-- Pattern matching revisited
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^

-- An example function using guards ...

mystery :: Integer -> Integer -> Integer
mystery x y 
  | x==0        = y
  | otherwise   = x

--  ... or pattern matching

mystery' :: Integer -> Integer -> Integer
mystery' 0 y = y
mystery' x _ = x

-- To join two strings

joinStrings :: (String,String) -> String
joinStrings (st1,st2) = st1 ++ "\t" ++ st2


-- Lists and list patterns
-- ^^^^^^^^^^^^^^^^^^^^^^^
-- From the Prelude ...

head             :: [a] -> a
head (x:_)        = x

tail             :: [a] -> [a]
tail (_:xs)       = xs

null             :: [a] -> Bool
null []           = True
null (_:_)        = False


-- The case construction
-- ^^^^^^^^^^^^^^^^^^^^^

-- Return the first digit in a string.

firstDigit :: String -> Char

firstDigit st 
  = case (digits st) of
      []    -> '\0'
      (x:_) -> x


-- Primitive recursion over lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The sum of a list of Ints.

sum :: [Integer] -> Integer

sum []     = 0
sum (x:xs) = x + sum xs

-- Property to test the re-implementation of sum
-- against the version in the prelude.

prop_sum :: [Integer] -> Bool

prop_sum xs =  sum xs == Prelude.sum xs

-- Finding primitive recursive definitions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- Concatenating a list of lists.

concat :: [[a]] -> [a]

concat []     = []
concat (x:xs) = x ++ concat xs

-- Joining two lists

(++) :: [a] -> [a] -> [a]

[]     ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)

-- Testing whether something is a member of a list.

-- Renamed to elem' as we use the elem from Prelude
-- elsewhere in the file.

elem' :: Integer -> [Integer] -> Bool

elem' x []     = False
elem' x (y:ys) = (x==y) || (elem' x ys)


-- To double every element of an integer list

doubleAll :: [Integer] -> [Integer]

doubleAll xs = [ 2*x | x<-xs ]

doubleAll' []     = []
doubleAll' (x:xs) = 2*x : doubleAll' xs

-- To select the even elements from an integer list. 

selectEven :: [Integer] -> [Integer]

selectEven xs = [ x | x<-xs , isEven x ]

selectEven' [] = []
selectEven' (x:xs)
  | isEven x    = x : selectEven' xs
  | otherwise   =     selectEven' xs

-- To sort a list of numbers into ascending order.

iSort :: [Integer] -> [Integer]

iSort []     = [] 
iSort (x:xs) = ins x (iSort xs) 

-- To insert an element at the right place into a sorted list.

ins :: Integer -> [Integer] -> [Integer]

ins x []    = [x] 
ins x (y:ys) 
  | x <= y      = x:(y:ys)
  | otherwise   = y : ins x ys


-- General recursions over lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


-- Zipping together two lists.

zip :: [a] -> [b] -> [(a,b)]

zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip (x:xs) []     = []
zip []     zs     = []

-- Taking a given number of elements from a list.

take :: Int -> [a] -> [a]

take 0 _        = []
take _ []       = []
take n (x:xs)
  | n>0         = x : take (n-1) xs
take _ _        = error "PreludeList.take: negative argument"

-- Quicksort over lists.

qSort :: [Integer] -> [Integer]

qSort [] = []
qSort (x:xs) 
  = qSort [ y | y<-xs , y<=x] ++ [x] ++ qSort [ y | y<-xs , y>x]



-- Example: Text Processing
-- ^^^^^^^^^^^^^^^^^^^^^^^^

-- The `whitespace' characters.

whitespace :: String
whitespace = ['\n','\t',' ']

-- Get a word from the front of a string.

getWord :: String -> String
getWord []    = [] 
getWord (x:xs) 
  | elem x whitespace   = []
  | otherwise           = x : getWord xs

-- In a similar way, the first word of a string can be dropped.

dropWord :: String -> String
dropWord []    = []
dropWord (x:xs) 
  | elem x whitespace   = (x:xs)
  | otherwise           = dropWord xs

-- To remove the whitespace character(s) from the front of a string.

dropSpace :: String -> String
dropSpace []    = []
dropSpace (x:xs) 
  | elem x whitespace   = dropSpace xs
  | otherwise           = (x:xs)

-- A word is a string.

type Word = String

-- Splitting a string into words.

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st
  = (getWord st) : split (dropSpace (dropWord st))

-- Splitting into lines of length at most lineLen

lineLen :: Int
lineLen = 80

-- A line is a list of words.

type Line = [Word]

-- Getting a line from a list of words.

getLine :: Int -> [Word] -> Line
getLine len []     = []
getLine len (w:ws)
  | length w <= len     = w : restOfLine  
  | otherwise           = []
    where
    newlen      = len - (length w + 1)
    restOfLine  = getLine newlen ws

-- Dropping the first line from a list of words.

dropLine :: Int -> [Word] -> Line

dropLine = dropLine 	-- DUMMY DEFINITION

-- Splitting into lines.

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws
  = getLine lineLen ws
         : splitLines (dropLine lineLen ws)

-- To fill a text string into lines, we write

fill :: String -> [Line]
fill = splitLines . splitWords
