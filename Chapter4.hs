--------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 4
--
--------------------------------------------------------------------------

-- NOTE
--
-- Added HUnit and QuickCheck tests
--
-- HUnit 1.0 documentation is out of date
-- re package name.

module Chapter4 where

import Test.HUnit
import Test.QuickCheck
import PicturesSVG hiding (test2)

-- Designing a program in Haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

maxThree :: Int -> Int -> Int -> Int
maxThree x y z = (x `max` y) `max` z

testMax1 = TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMax2 = TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMax3 = TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMax4 = TestCase (assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6))

-- run as 
--   runTestTT testsMax

testsMax = TestList [testMax1, testMax2, testMax3, testMax4]

-- NOTE
--
-- Added this type synonym so that can switch easily
-- between Integer and Int.

type MyNum = Integer

middleNumber :: MyNum -> MyNum -> MyNum -> MyNum
middleNumber x y z
  | between y x z      = x
  | between x y z      = y
  | otherwise          = z

-- What follows here is a dummy definition of between; you need to replace this
-- with a proper definition for the function middleNumber to work.

between ::  MyNum -> MyNum -> MyNum -> Bool

-- dummy definition 
-- for you to complete!

between = between


-- NOTE
--
-- HUnit tests added
--
-- To run evaluate: runTestTT tests

test1 = TestCase (assertEqual "for: between 2 3 4" True (between 2 3 4))
test2 = TestCase (assertEqual "for: between 2 3 2" False (between 2 3 2))
test3 = TestCase (assertEqual "for: between 2 3 3" True (between 2 3 3))
test4 = TestCase (assertEqual "for: between 3 3 3" True (between 3 3 3))
test5 = TestCase (assertEqual "for: between 3 2 3" False (between 3 2 3))
test6 = TestCase (assertEqual "for: between 3 2 1" True (between 3 2 1))

testsBetween = TestList [test1, test2, test3, test4, test5, test6]

-- NOTE
-- 
-- Interesting to vary the implementation and see which tests fail.
-- Simple form of mutation testing.

-- QuickCheck test
--
-- Does the tricky implementation of between work in the 
-- same way as the case analysis?

prop_between :: MyNum -> MyNum -> MyNum -> Bool

prop_between x y z 
 = (between x y z) == ((x<=y)&&(y<=z))||((x>=y)&&(y>=z))

-- Unit tests as Quick Check properties

prop_between1 :: Bool

prop_between1
 = between 2 3 4 == True

-- Local definitions
-- ^^^^^^^^^^^^^^^^^

-- Four ways of defining a Picture using 
-- different combinations of loca definitions.


fourPics1 :: Picture -> Picture

fourPics1 pic =
    left `beside` right
      where
        left  = pic `above` invertColour pic
        right = invertColour (flipV pic) `above` flipV pic

fourPics2 :: Picture -> Picture
fourPics2 pic =
    left `beside` right
      where
        left    = pic `above` invertColour pic
        right   = invertColour flipped `above` flipped
        flipped = flipV pic

fourPics3 :: Picture -> Picture

fourPics3 pic =
    left `beside` right
      where
        left  = pic `above` invertColour pic
        right = invertColour (flipV left)

fourPics4 :: Picture -> Picture

fourPics4 pic =
    left `beside` right
      where
        stack p  = p `above` invertColour p
        left     = stack pic
        right    = stack (invertColour (flipV pic))

-- Area of a triangle

triArea' :: Float -> Float -> Float -> Float

triArea' a b c 
    | possible   = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise  = 0
    where
      s = (a+b+c)/2 
      possible = possible -- dummy definition

-- Sum of squares

sumSquares :: Integer -> Integer -> Integer

sumSquares n m 
  = sqN + sqM
    where
    sqN = n*n
    sqM = m*m


-- Let expressions
-- ^^^^^^^^^^^^^^^

-- Two examples which use `let'.

letEx1 :: Integer
letEx1 = let x = 3+2 in x^2 + 2*x - 4

letEx2 :: Integer
letEx2 = let x = 3+2 ; y = 5-1 in x^2 + 2*x - y


-- Scopes

isOdd, isEven :: Int -> Bool

isOdd n 
  | n<=0        = False
  | otherwise   = isEven (n-1)

isEven n 
  | n<0         = False
  | n==0        = True
  | otherwise   = isOdd (n-1)


-- Defining types for ourselves
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Rock - Paper - Scissors

data Move = Rock | 
            Paper | 
            Scissors
            deriving Eq

-- Showing Moves in an abbreviated form.

instance Show Move where
      show Rock = "r"
      show Paper = "p"
      show Scissors = "s"

-- For QuickCheck to work over the Move type.

instance Arbitrary Move where
  arbitrary     = elements [Rock, Paper, Scissors]

-- Calculating the Move to beat or lose against the 
-- argument Move.

beat, lose :: Move -> Move

beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper


-- Primitive recursion over Int
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The factorial of n is 1*2*...*(n-1)*n, so that factorial of four is 24.
-- It is often written n!

fac :: Integer -> Integer
fac n
  | n==0        = 1
  | n>0         = fac (n-1) * n
  | otherwise   = error "fac only defined on natural numbers"

--                                      n
-- Raising two to a power: power2 n is 2  in mathematical notation.

power2 :: Integer -> Integer
power2 n
  | n==0        = 1
  | n>0         = 2 * power2 (n-1)

-- The sum of the factorials up to a particular value, 0! + 1! + ... n!.

sumFacs :: Integer -> Integer
sumFacs n
  | n==0        = 1
  | n>0         = sumFacs (n-1) + fac n  

-- The sum of the values of a function up to a particular value: 
-- 	f 0 + f 1 + ... f n
-- from which you can reconstruct sumFacs: sumFacs n = sumFun fac n

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
  | n==0        = f 0
  | n>0         = sumFun f (n-1) + f n  

-- The maximum number of regions into which n lines can cut a plane.

regions :: Integer -> Integer 
regions n
  | n==0        = 1
  | n>0         = regions (n-1) + n

-- The Fibonacci numbers 0, 1, 1, 2, 3, 5, ..., u, v, u+v, ...

fib :: Integer -> Integer
fib n 
  | n==0        = 0
  | n==1        = 1
  | n>1         = fib (n-2) + fib (n-1)

-- Division of integers

remainder :: Integer -> Integer -> Integer
remainder m n 
  | m<n         = m
  | otherwise   = remainder (m-n) n

divide    :: Integer -> Integer -> Integer
divide m n
  | m<n         = 0
  | otherwise   = 1 + divide (m-n) n

-- Testing
-- ^^^^^^^

-- Does this function calculate the maximum of three numbers?

mysteryMax :: Integer -> Integer -> Integer -> Integer
mysteryMax x y z
  | x > y && x > z      = x
  | y > x && y > z      = y
  | otherwise           = z

testMMax1 = TestCase (assertEqual "for: mysteryMax 6 4 1" 6 (mysteryMax 6 4 1))
testMMax2 = TestCase (assertEqual "for: mysteryMax 6 6 6" 6 (mysteryMax 6 6 6))
testMMax3 = TestCase (assertEqual "for: mysteryMax 2 6 6" 6 (mysteryMax 2 6 6))
testMMax4 = TestCase (assertEqual "for: mysteryMax 2 2 6" 6 (mysteryMax 2 2 6))
testMMax5 = TestCase (assertEqual "for: mysteryMax 6 6 2" 6 (mysteryMax 6 6 2))


testsMMax = TestList [testMMax1, testMMax2, testMMax3, testMMax4, testMMax5]


-- Numbers of roots

numberNDroots :: Float -> Float -> Float -> Integer 

numberNDroots a b c
    | bsq > fac   = 2
    | bsq == fac  = 1
    | bsq < fac   = 0
    where
      bsq = b*b
      fac = 4.0*a*c 

-- Area of a triangle

triArea :: Float -> Float -> Float -> Float

triArea a b c 
    | possible a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise      = 0
    where
      s = (a+b+c)/2 

possible :: Float -> Float -> Float -> Bool

possible a b c = True -- dummy definition

fact :: Int -> Int
  
fact n 
    | n>1       = n * fact (n-1)
    | otherwise = 1

prop_fact n =
  fact n > 0

-- Extended exercise
-- ^^^^^^^^^^^^^^^^^

blackSquares :: Integer -> Picture

blackSquares n
  | n<=1	     = black
  | otherwise = black `beside` blackSquares (n-1)

blackWhite :: Integer -> Picture

blackWhite n
  | n<=1	     = black
  | otherwise = black `beside` whiteBlack (n-1)

whiteBlack = error "exercise for you"

blackChess :: Integer -> Integer -> Picture

blackChess n m
  | n<=1	     = blackWhite m
  | otherwise = blackWhite m `above` whiteChess (n-1) m

whiteChess n m = error "exercise for you"