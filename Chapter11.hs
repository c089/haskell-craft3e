-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 11
--
-----------------------------------------------------------------------



-- Functions as values
-- ^^^^^^^^^^^^^^^^^^^

module Chapter11 where

import Prelude hiding (succ,curry,uncurry,flip)
import Chapter10 (getUntil) 
import Chapter7 (whitespace) 
import Test.QuickCheck

-- A fixity declaration for the forward composition operator, >.>

infixl 9 >.>


-- Function composition and forward composition
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A composition operator taking its arguments in the opposite order to `.'.


(>.>) :: (a -> b) -> (b -> c) -> (a -> c)

g >.> f = f . g


-- Expressions for functions: lambda abstractions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

addOnes :: [Integer]

addOnes = map (\x -> x+1) [2,3,4]


-- Mapping a list of functions onto a value

mapFuns :: [a->b] -> a -> [b]

mapFuns [] x     = []
mapFuns (f:fs) x = f x : mapFuns fs x

-- Two alternative definitions

mapFuns1 fs x = map (\f -> f x) fs

mapFuns2 fs x = map applyToX fs
			   where
			   applyToX f = f x

-- A function returning a function, namely the function to `add n to its
-- argument'.

addNum :: Integer -> (Integer -> Integer)

addNum n = (\m -> n+m)

-- The `plumbing' function:

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)

comp2 f g = (\x y -> g (f x) (f y))

-- Using the `plumbing' function

plumbingExample = comp2 sq add 3 4
		  where
		  sq x    = x*x
		  add y z = y+z

 
-- Partial Application
-- ^^^^^^^^^^^^^^^^^^^

-- The function multiply multiplies together two arguments.

multiply :: Int -> Int -> Int
multiply x y = x*y

-- Double all elements of an integer list.

doubleAll :: [Int] -> [Int]
doubleAll = map (multiply 2)

-- Another definition of addNum, using partial application to achieve the
-- `function as result'.

addNum' n m = n+m

-- Operator  Sections

-- Example of a function defined using partial application and operator sections.

egFun :: [Int] -> [Int]

egFun = filter (>0) . map (+1)



-- Three examples from the text processing functions first seen in Chapter 7.

dropSpace = dropWhile (member whitespace)
dropWord  = dropWhile (not . member whitespace)
getWord   = takeWhile (not . member whitespace)

-- Auxiliary definitions ...
 
member xs x = elem x xs

-- Under the hood: curried functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- An example function of type (Int -> Int) -> Int

g :: (Int -> Int) -> Int
g h = (h 0) + (h 1)

-- Currying and uncurrying
-- ^^^^^^^^^^^^^^^^^^^^^^^

-- An uncurried function to multiply together the two itegers in a pair.

multiplyUC :: (Int,Int) -> Int
multiplyUC (x,y) = x*y

-- Turn an uncurried function into a curried version,

curry :: ((a,b) -> c) -> (a -> b -> c)
curry g x y = g (x,y)

-- and vice versa.

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

-- Zip property

prop_zip :: [(Integer, Integer)] -> Bool
prop_zip xs = uncurry zip (unzip xs) == xs

-- Defining higher-order functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Using the operators

-- Compose a function with itself: apply it twice, in other words.

twice :: (a -> a) -> (a -> a)
twice f = (f . f)

succ :: Int -> Int
succ n = n+1

-- We can generalize twice so that we pass a parameter giving the number
-- of times the functional argument is to be composed with itself:

iter :: Int -> (a -> a) -> (a -> a)

iter n f 
  | n>0         = f . iter (n-1) f
  | otherwise   = id

-- An alternative definition of iter:

iter' n f = foldr (.) id (replicate n f)

-- Using local definitions

addNum2 :: Integer -> Integer -> Integer

addNum2 n = addN
		   where
		   addN m = n+m

addNum3 n = let 
             addN m = n+m
           in
             addN

-- Lambda abstractions

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x

-- Change the order of arguments of a two argument curried function.

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

-- Mystery function from "Point-free programming"

puzzle = (.) (.)

-- Final examples

-- Double all integers in a list,

doubleAll' :: [Int] -> [Int]
doubleAll' = map (*2)

-- get the even numbers in a list of integers,

getEvens :: [Int] -> [Int]
getEvens = filter ((==0).(`mod` 2))

-- get a word from the start of a string.

getWord' = getUntil (`elem` whitespace)
 




-- Verification and general functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

prop_mf p f = 
    \xs -> (filter p . map f) xs == (map f . filter (p . f)) xs
