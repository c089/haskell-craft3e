--------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 14, part 2
--      Details of the Simulation case study in the Simulation directory.
--
--------------------------------------------------------------------

module Chapter14_2 where

import Prelude hiding (Either(..),either,Maybe(..),maybe)
import Chapter14_1 hiding (Name,NTree(..))
import Test.QuickCheck
import Control.Monad

-- Algebraic types, part 2
-- ^^^^^^^^^^^^^^^^^^^^^^^


-- Polymorphic algebraic types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A type of pairs of elements, taken from the same type.

data Pairs a = Pr a a

-- and example elements of the type are

pair1 = Pr 2 3    :: Pairs Int
pair2 = Pr [] [3] :: Pairs [Int]
pair3 = Pr [] []  :: Pairs [a]

-- Are the two halves equal?

equalPair :: Eq a => Pairs a -> Bool
equalPair (Pr x y) = (x==y)


-- Lists
-- ^^^^^

-- Defining lists from scratch (which loses some of the special syntax for
-- lists).

infixr 5 :::

data List a = NilL | a ::: (List a)
              deriving (Eq,Ord,Show,Read)

-- Binary trees
-- ^^^^^^^^^^^^


-- Binary trees carrying elements of an arbitrary type.

data Tree a = Nil | Node a (Tree a) (Tree a)
              deriving (Eq,Ord,Show,Read)

-- The depth of a binary tree.

depthT :: Tree a -> Integer
depthT Nil            = 0
depthT (Node n t1 t2) = 1 + max (depthT t1) (depthT t2)

-- Turning a tree into a list.

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node x t1 t2)
  = collapse t1 ++ [x] ++ collapse t2
--  
-- For example,
--  

collapseEG 
 = collapse (Node 12 
               (Node 34 Nil Nil) 
               (Node 3 (Node 17 Nil Nil) Nil))

-- Mapping a function over all elements in a tree, preserving the
-- structure.

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node x t1 t2)
  = Node (f x) (mapTree f t1) (mapTree f t2)


-- The union type, Either
-- ^^^^^^^^^^^^^^^^^^^^^^

-- A union type -- defined in the Prelude.

data Either a b = Left a | Right b
                  deriving (Eq,Ord,Read,Show)

-- Examples

eitherEG1 = Left "Duke of Prunes" :: Either String Int
eitherEG2 = Right 33312           :: Either String Int

-- In the left or the right?

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- To define a function from Either a b to c we have to deal with two cases,

either :: (a -> c) -> (b -> c) -> Either a b -> c

either f g (Left x)  = f x
either f g (Right y) = g y


-- If we have a function f::a -> cand we wish to apply it to an element
-- of Either a b, there is a problem: what do we do if the element is
-- in the right-hand side of the Either type? A simple answer is to raise an error

applyLeft :: (a -> c) -> Either a b -> c

applyLeft f (Left x)  = f x
applyLeft f (Right _) = error "applyLeft applied to Right"

-- Arbitrarily branching trees

data GTree a = Leaf a | Gnode [GTree a]


-- Case study: Program Errors
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^

-- This section explores various ways of handling errors raised in program
-- execution.

-- \subsection*{Dummy Values}
-- \index{dummy values at errors}

-- The tail function re-defined to give an empty list when applied to the empty list. 

tl :: [a] -> [a]
tl (_:xs) = xs
tl []     = []

-- Zero returned when division by zero,

divide :: Integer -> Integer -> Integer
divide n m 
  | (m /= 0)    = n `div` m
  | otherwise   = 0

-- Head redefined to give a dummy value on the empty list; the value has
-- to be a parameter.

hd :: a -> [a] -> a
hd y (x:_) = x
hd y []    = y

-- Error types
-- ^^^^^^^^^^^

-- The Maybe type, as defined in the Prelude.lhs,

data Maybe a = Nothing | Just a
               deriving (Eq,Ord,Read,Show)

-- An error-raising division function

errDiv :: Integer -> Integer -> Maybe Integer
errDiv n m 
  | (m /= 0)    = Just (n `div` m)
  | otherwise   = Nothing 

-- The function mapMaybe transmits an error value though the application of
-- the function g. 

mapMaybe :: (a -> b) -> Maybe a -> Maybe b

mapMaybe g Nothing  = Nothing
mapMaybe g (Just x) = Just (g x)

-- In trapping an error, we aim to return a result of type b, from an
-- input of type Maybe a; there are two cases to deal with:
-- normal result (Just); error (Nothing).

maybe :: b -> (a -> b) -> Maybe a -> b

maybe n f Nothing  = n
maybe n f (Just x) = f x

-- Examples

handle1, handle2 :: Integer
handle1 = maybe 56 (1+) (mapMaybe (*3) (errDiv 9 0)) 
handle2 = maybe 56 (1+) (mapMaybe (*3) (errDiv 9 1))  

-- Generalising the Maybe type to include an error message in the `Nothing'
-- part.

data Err a = OK a | Error String


-- Design with Algebraic Data Types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Case study: edit distance
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- A type to represent the different sorts of Edit operations.

data Edit = Change Char |
            Copy |
            Delete |
            Insert Char |
            Kill  
            deriving (Eq,Show)

-- Transforming one string into another, optimally,

transform :: String -> String -> [Edit]

transform [] [] = []
transform xs [] = [Kill]
transform [] ys = map Insert ys
transform (x:xs) (y:ys)
  | x==y        = Copy : transform xs ys
  | otherwise   = best [ Delete   : transform xs (y:ys) ,
                         Insert y : transform (x:xs) ys ,
                         Change y : transform xs ys ]
--  
-- How do we choose the best sequence? We choose the one with the lowest
-- cost.

best :: [[Edit]] -> [Edit]

best [x]   = x
best (x:xs) 
  | cost x <= cost b    = x
  | otherwise           = b
      where 
      b = best xs

-- The cost is given by charging one for every operation except copy,
-- which is equivalent to `leave unchanged'.

cost :: [Edit] -> Int

cost = length . filter (/=Copy)

-- For testing purposes: does the best actually do the job: need to be
-- able to apply a list of edits to transform a string

edit :: [Edit] -> String -> String

edit [] string = string
edit (e:es) [] = 
    case e of 
      Insert ch -> ch : edit es []
      Kill -> []

edit (e:es) string@(x:xs) =
    case e of 
      Change ch -> ch : edit es xs
      Copy -> x : edit es xs
      Delete -> edit es xs
      Insert ch -> ch : edit es string
      Kill -> []

-- Simulation
-- ^^^^^^^^^^

-- NOTE: details of the Simulation case study are collected separately.

--  
-- Algebraic types and type classes
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


-- Movable objects
-- ^^^^^^^^^^^^^^^

data Vector = Vec Float Float

class Movable a where
  move      :: Vector -> a -> a
  reflectX  :: a -> a
  reflectY  :: a -> a
  rotate180 :: a -> a
  rotate180 = reflectX . reflectY

data Point = Point Float Float 
             deriving Show

instance Movable Point where
  move (Vec v1 v2) (Point c1 c2) = Point (c1+v1) (c2+v2)
  reflectX (Point c1 c2)  = Point c1 (-c2)
  reflectY (Point c1 c2)  = Point (-c1) c2
  rotate180 (Point c1 c2) = Point (-c1) (-c2)

data Figure = Line Point Point |
              Circle Point Float 
              deriving Show

instance Movable Figure where
  move v (Line p1 p2) = Line (move v p1) (move v p2)
  move v (Circle p r) = Circle (move v p) r

  reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
  reflectX (Circle p r) = Circle (reflectX p) r

  reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
  reflectY (Circle p r) = Circle (reflectY p) r

instance Movable a => Movable [a] where
  move v   = map (move v)
  reflectX = map reflectX
  reflectY = map reflectY


-- Named objects
-- ^^^^^^^^^^^^^

-- Named objects:

class Named a where
  lookName :: a -> String
  giveName :: String -> a -> a

-- A named type ...

data Name a = Pair a String

-- ... as witnessed by the instance declaration.

instance Named (Name a) where
  lookName (Pair obj nm) = nm
  giveName nm (Pair obj _) = (Pair obj nm)

-- Putting together classes
-- ^^^^^^^^^^^^^^^^^^^^^^^^

-- See the text for details of what is going on here.

mapName :: (a -> b) -> Name a -> Name b

mapName f (Pair obj nm) = Pair (f obj) nm

instance Movable a => Movable (Name a) where
  move v   = mapName (move v)
  reflectX = mapName reflectX
  reflectY = mapName reflectY

class (Movable b, Named b) => NamedMovable b

instance Movable a => NamedMovable (Name a)




-- Reasoning about algebraic types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The functions discussed here are all defined elsewhere.


-- QuickCheck for algebraic types

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)

arbTree 0 = return Nil
arbTree n
    | n>0
        = frequency[(1, return Nil),
                    (3, liftM3 Node arbitrary bush bush)]
          where
            bush = arbTree (div n 2)

-- collapse, map and mapTree

prop_collapse :: Eq b => (a -> b) -> Tree a -> Bool

prop_collapse f =
    \t -> map f (collapse t) == collapse (mapTree f t)

-- two different ways of measuring the size of a tree

prop_sizeT :: Tree a -> Bool

prop_sizeT t =
    sizeT t == (leavesT t) + length (collapse t)

-- functions used above: count the number of leaves
-- and the overall size of the tree ...

leavesT :: Tree a -> Int

leavesT Nil = 1
leavesT (Node _ t1 t2) = leavesT t1 + leavesT t2

sizeT :: Tree a -> Int

sizeT Nil = 1
sizeT (Node _ t1 t2) = 1 + sizeT t1 + sizeT t2

-- edit distance

-- does the transform actually do the right transformation?

prop_transform :: String -> String -> Property

prop_transform xs ys =
     length (xs++ys) <= 15 ==> edit (transform xs ys) xs == ys

-- is it short enough?

prop_transformLength :: String -> String -> Property

prop_transformLength xs ys =
    length (xs++ys) <= 15 ==> cost (transform xs ys) <= length ys + 1
