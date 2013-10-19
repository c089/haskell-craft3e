------------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 10
--
-------------------------------------------------------------------------

-- Generalization: patterns of computation
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

module Chapter10 where

import Prelude hiding (map,filter,zipWith,foldr1,foldr,concat,and)
import Pictures hiding (flipV,beside)
import qualified Chapter7 

-- Higher-order functions: functions as arguments
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Mapping a function along a list.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

map,map' :: (a -> b) -> [a] -> [b]

map' f xs = [ f x | x <- xs ]				-- (map.0)

map f []     = []					-- (map.1)
map f (x:xs) = f x : map f xs				-- (map.2)

-- Examples using map.

-- Double all the elements of a list ...

doubleAll :: [Integer] -> [Integer]

doubleAll xs = map double xs	       
	       where	
	       double x = 2*x
 
-- ... convert characters to their numeric codes ...

convertChrs :: [Char] -> [Int]
convertChrs xs = map fromEnum xs

-- ... flip a Picture in a vertical mirror.

flipV :: Picture -> Picture
flipV xs = map reverse xs


-- Modelling properties as functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Is an integer even?

isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)

-- Is a list sorted?

isSorted :: [Integer] -> Bool
isSorted xs = (xs == iSort xs)


-- Filtering -- the filter function
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

filter :: (a -> Bool) -> [a] -> [a]

filter p [] = []				-- (filter.1)
filter p (x:xs)
  | p x         = x : filter p xs		-- (filter.2)
  | otherwise   =     filter p xs		-- (filter.3)

-- A list comprehension also serves to define filter,

filter' p xs = [ x | x <- xs , p x ]		-- (filter.0)


-- Combining zip and map -- the zipWith function
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f  _      _     = []

beside :: Picture -> Picture -> Picture
beside pic1 pic2 = zipWith (++) pic1 pic2


-- Folding and primitive recursion
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Folding an operation into a non-empty list

foldr1 :: (a -> a -> a) -> [a] -> a

foldr1 f [x]    = x				-- (foldr1.1)
foldr1 f (x:xs) = f x (foldr1 f xs)		-- (foldr1.2)

-- Examples using foldr1

foldEx1 = foldr1 (+) [3,98,1]
foldEx2 = foldr1 (||) [False,True,False]
foldEx3 = foldr1 (++) ["Freak ", "Out" , "", "!"] 
foldEx4 = foldr1 min [6]
foldEx5 = foldr1 (*) [1 .. 6]

-- Folding into an arbitrary list: using a starting value on the empty list.

foldr f s []     = s				-- (foldr.1)
foldr f s (x:xs) = f x (foldr f s xs)		-- (foldr.2)

-- Concatenating a list using foldr.

concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

-- Conjoining a list of Bool using foldr.

and :: [Bool] -> Bool
and bs = foldr (&&) True bs

-- Can define foldr1 using foldr:
-- 	foldr1 f (x:xs) = foldr f x xs			-- (foldr1.0)


-- Folding in general -- foldr again
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The type of foldr is more general than you would initially expect...

foldr :: (a -> b -> b) -> b -> [a] -> b

rev :: [a] -> [a]
rev xs = foldr snoc [] xs

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- Sorting a list using foldr

iSort :: [Integer] -> [Integer]
iSort xs = foldr Chapter7.ins [] xs

-- From the exercises: a mystery function ...

mystery xs = foldr (++) [] (map sing xs)
sing x     = [x]


-- Generalizing: splitting up lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Getting the first word from the front of a String ...

getWord :: String -> String
getWord []    = [] 					-- (getWord.1)
getWord (x:xs) 
  | elem x Chapter7.whitespace  = []			-- (getWord.2)
  | otherwise           	= x : getWord xs 	-- (getWord.3)

-- ... which generalizes to a function which gets items from the front of a list
-- until an item has the required property.

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p []    = [] 
getUntil p (x:xs) 
  | p x         = []
  | otherwise   = x : getUntil p xs

-- The original getWord function defined from getUntil

-- 	getWord xs 
-- 	  = getUntil p xs
-- 	    where 
-- 	    p x = elem x whitespace

