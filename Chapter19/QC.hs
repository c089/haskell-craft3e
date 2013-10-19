-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	QC.hs
--
--      Generating values randomly.
--
-----------------------------------------------------------------------

module QC where

import Test.QuickCheck

import Control.Monad (liftM,liftM2)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub)
import QCfuns -- to Show functions

-- Simple examples for data generation

data Card = Card Int String
            deriving (Eq,Show)

data Info = Number Int | Email String
            deriving (Eq, Show)

data List a = Empty | Cons a (List a)
            deriving (Eq, Show)

instance Arbitrary Card where
    arbitrary =
        do
          int <- arbitrary
          string <- arbitrary
          return (Card int string)

instance Arbitrary Info where
    arbitrary =
        do
          boo <- arbitrary
          if boo
            then do
              int <- arbitrary
              return (Number int) 
            else do
              string <- arbitrary
              return (Email string) 

-- Generating lists of samples

-- instance Arbitrary a => Arbitrary (List a) where
--     arbitrary =
--         do
--           boo <- elements [True, False]
--           if boo
--                   then 
--                     return $ Empty 
--                   else do
--                     val  <- arbitrary
--                     list <- arbitrary
--                     return $ Cons val list 

instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        do
          switch <- elements [1,2,3]
          case switch of 
            1 -> return Empty 
            _ -> 
                do
                  val  <- arbitrary
                  list <- arbitrary
                  return (Cons val list) 

-- The expr type from the calculator

data Expr = Lit Integer |
            Add Expr Expr |
            Sub Expr Expr
                deriving (Show,Eq)

instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr

arbExpr 0 = liftM Lit arbitrary

arbExpr n = frequency
    [(1, liftM Lit arbitrary),
     (2, liftM2 Add subExp subExp),
     (2, liftM2 Sub subExp subExp)]
        where
          subExp = arbExpr (div n 2)
{-
arbExpr 0 = 
    do int <- arbitrary
       return (Lit int)

arbExpr n
    | n>0 =
        do
          pick <- choose (0,2::Int)
          case pick of
            0 -> do 
              int <- arbitrary
              return (Lit int)
            1 -> do 
              left  <- subExp
              right <- subExp
              return (Add left right)
            2 -> do 
              left  <- subExp
              right <- subExp
              return (Sub left right)
        where
          subExp = arbExpr (div n 2)
-}

prettyE :: Expr -> String

prettyE (Lit n) = show n
prettyE (Add e1 e2) = "("++prettyE e1 ++"+"++prettyE e2 ++")"
prettyE (Sub e1 e2) = "("++prettyE e1 ++"-"++prettyE e2 ++")"

-- Property of map

prop_map f g xs =
  map (f::Int->Int) (map (g::Int -> Int) xs) == map (g.f) xs

