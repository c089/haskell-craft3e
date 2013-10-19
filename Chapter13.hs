-----------------------------------------------------------------------
--
--	Haskell: The Craft of Functional Programming, 3e
--	Simon Thompson
--	(c) Addison-Wesley, 1996-2011.
--
--	Chapter 13
--
-----------------------------------------------------------------------

module Chapter13 where

import Data.List
import Chapter5 (Shape(..),area)

-- Overloading and type classes
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Why overloading?
-- ^^^^^^^^^^^^^^^^

-- Testing for membership of a Boolean list.

elemBool :: Bool -> [Bool] -> Bool

elemBool x [] = False
elemBool x (y:ys)
  = (x == y) || elemBool x ys

-- Testing for membership of a general list, with the equality function as a
-- parameter.

elemGen :: (a -> a -> Bool) -> a -> [a] -> Bool

elemGen eqFun x [] = False
elemGen eqFun x (y:ys)
  = (eqFun x y) || elemGen eqFun x ys


-- Introducing classes
-- ^^^^^^^^^^^^^^^^^^^

-- Definitions of classes cannot be hidden, so the definitions etc. here are not
-- executable.

-- class Eq a where
--   (==) :: a -> a -> Bool

-- Functions which use equality
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Testing for three values equal: more general than Int -> Int -> Int -> Bool.

allEqual :: Eq a => a -> a -> a -> Bool
allEqual m n p = (m==n) && (n==p)

-- Erroneous expression

-- error1 = allEqual suc suc suc

suc = (+1)

-- elem :: Eq a => a -> [a] -> Bool
-- books :: Eq a => [ (a,b) ] -> a -> [b]

-- It is easier to see this typing if you remane books lookupFirst:

lookupFirst :: Eq a => [ (a,b) ] -> a -> [b]

lookupFirst ws x 
  = [ z | (y,z) <- ws , y==x ]

-- borrowed    :: Eq b => [ (a,b) ] -> b -> Bool
-- numBorrowed :: Eq a => [ (a,b) ] -> a -> Int


-- Signatures and Instances
-- ^^^^^^^^^^^^^^^^^^^^^^^^

-- A type is made a member or instance of a class by defining
-- the signature functions for the type. For example,

-- instance Eq Bool where
--   True  == True  = True
--   False == False = True
--   _     == _     = False

-- The Info class:

class Info a where
  examples :: [a]
  size     :: a -> Int
  size _   = 1

-- Declaring instances of the Info class


instance Info Int where
  examples = [-100..100]
   --size _   = 1

instance Info Char where
  examples = ['a','A','z','Z','0','9']
  -- size _   = 1

instance Info Bool where
  examples = [True,False]
  -- size _   = 1

-- An instance declaration for a data type.

instance Info Shape where
  examples = [ Circle 3.0, Rectangle 45.9 87.6 ]
  size     = round . area


-- Instance declaration with contexts.

instance Info a => Info [a] where
  examples = [ [] ] ++ [ [x] | x<-examples ] ++ [ [x,y] | x<-examples , y<-examples ]
  size     = foldr (+) 1 . map size  

instance (Info a,Info b) => Info (a,b) where
  examples   = [ (x,y) | x<-examples , y<-examples ]
  size (x,y) = size x + size y + 1 


-- Default definitions
-- ^^^^^^^^^^^^^^^^^^^

-- To return to our example of equality, the Haskell equality class is in fact
-- defined by

-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x /= y     = not (x==y)
--   x == y     = not (x/=y)


-- Derived classes
-- ^^^^^^^^^^^^^^^

-- Ordering is built on Eq.

-- class Eq a => Ord a where
--   (<), (<=), (>), (>=) :: a -> a -> Bool
--   max, min             :: a -> a -> a
--   compare              :: a -> a -> Ordering


-- This is the same definition as in Chapter7, but now with an overloaded type.

iSort :: Ord a => [a] -> [a]

iSort []	= []
iSort (x:xs) = ins x (iSort xs)

-- To insert an element at the right place into a sorted list.

ins :: Ord a => a -> [a] -> [a]

ins x []    = [x]
ins x (y:ys)
  | x <= y	= x:(y:ys)
  | otherwise	= y : ins x ys


-- Multiple constraints
-- ^^^^^^^^^^^^^^^^^^^^

-- Sorting visible objects ...

vSort :: (Ord a,Show a) => [a] -> String

vSort = show . iSort 

-- Similarly, 

vLookupFirst :: (Eq a,Show b) => [(a,b)] -> a -> String

vLookupFirst xs x = show (lookupFirst xs x)

-- Multiple constraints can occur in an instance declaration, such as

-- instance (Eq a,Eq b) => Eq (a,b) where
--   (x,y) == (z,w)  =  x==z && y==w

-- Multiple constraints can also occur in the definition of a class,

class (Ord a,Show a) => OrdVis a

-- Can then give vSort the type:

-- 	vSort :: OrdVis a => [a] -> String

-- InfoCheck. Check a property for all examples

-- infoCheck :: (Info a) => (a -> Bool) -> Bool

-- infoCheck property = and (map property examples)

class Checkable b where
 infoCheck :: (Info a) => (a -> b) -> Bool

instance Checkable Bool where
  infoCheck property = and (map property examples)  

instance (Info a, Checkable b) => Checkable (a -> b) where
  infoCheck property = and (map (infoCheck.property) examples) 

test0 = infoCheck (\x -> (x <=(0::Int) || x>0))
test1 = infoCheck (\x y -> (x <=(0::Int) || y <= 0 || x*y >= x))
test2 = infoCheck (\x y -> (x <=(0::Int) || y <= 0 || x*y > x))




-- A tour of the built-in Haskell classes
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- For details of the code here, please see the standard Prelude and Libraries.


-- Types and Classes
-- ^^^^^^^^^^^^^^^^^

-- The code in this section is not legal Haskell.

-- To evaluate the type of concat . map show, type

-- 	:type concat . map show

-- to the Hugs prompt.

-- Type checking and type inference
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

prodFun :: (t -> t1) -> (t -> t2) -> t -> (t1,t2)

prodFun f g = \x -> (f x, g x)



-- Checking types
-- ^^^^^^^^^^^^^^

-- Non-type-correct definitions are included as comments.

example1 = fromEnum 'c' + 3

-- 	example2 = fromEnum 'c' + False

-- 	f n     = 37+n
-- 	f True  = 34

-- 	g 0 = 37
-- 	g n = True

-- 	h x 
-- 	  | x>0         = True
-- 	  | otherwise   = 37

-- 	k x = 34
-- 	k 0 = 35


-- Polymorphic type checking
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- Examples without their types; use Hugs to find them out.

f (x,y) = (x , ['a' .. y])

g (m,zs) = m + length zs

h = g . f

expr :: Int
expr = length ([]++[True]) + length ([]++[2,3,4]) 

-- The funny function does not type check.

-- 	funny xs = length (xs++[True]) + length (xs++[2,3,4])


-- Type checking and classes
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- Membership on lists

member :: Eq a => [a] -> a -> Bool

member []     y = False
member (x:xs) y = (x==y) || member xs y

-- Merging ordered lists.

merge (x:xs) (y:ys) 
  | x<y         = x : merge xs (y:ys)
  | x==y        = x : merge xs ys
  | otherwise   = y : merge (x:xs) ys
merge (x:xs) []    = (x:xs)
merge []    (y:ys) = (y:ys)
merge []    []     = []
