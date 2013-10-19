--------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 6
--
--------------------------------------------------------------------------

module Chapter6 where

import Prelude hiding (id)
import Test.QuickCheck

-- Polymorphism
-- ^^^^^^^^^^^^

-- Defining the identity function

id :: a -> a

id x = x

-- Extracting the first element of a pair.

fst :: (a,b) -> a

fst (x,y) = x

-- A "mystery" function

mystery :: (Bool,a) -> Char
mystery (x,y) = if x then 'c' else 'd'


-- The Picture example, revisited.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The type of pictures.

type Picture = [[Char]]

-- To flip a
-- picture in a horizontal mirror, 

flipH :: Picture -> Picture
flipH = reverse

-- and to place one picture above another it is sufficient to join the two lists of
-- lines together.

above :: Picture -> Picture -> Picture
above = (++)

-- To flip a picture in a vertical mirror.

flipV :: Picture -> Picture
flipV pic 
  = [ reverse line | line <- pic ]

-- To place two pictures side by side. 

beside :: Picture -> Picture -> Picture
beside picL picR
  = [ lineL ++ lineR | (lineL,lineR) <- zip picL picR ]

-- To invert the colour of a single character ...

invertChar :: Char -> Char
invertChar ch 
  = if ch=='.' then '#' else '.'

-- a line ...

invertLine :: [Char] -> [Char]
invertLine line 
  = [ invertChar ch | ch <- line ]

-- and a picture.

invertColour :: Picture -> Picture
invertColour pic 
  = [ invertLine line | line <- pic ]

-- Alternative definition of invertColour:

invertColour' :: Picture -> Picture
invertColour' pic 
  = [ [ invertChar ch | ch <- line ] | line <- pic ]

-- Properties for Pictures
-- ^^^^^^^^^^^^^^^^^^^^^^^

prop_AboveFlipV, prop_AboveFlipH :: Picture -> Picture -> Bool

prop_AboveFlipV pic1 pic2 = 
    flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2) 

prop_AboveFlipH pic1 pic2 = 
    flipH (pic1 `above` pic2) == (flipH pic1) `above` (flipH pic2) 

propAboveBeside :: Picture -> Picture ->  Picture -> Picture -> Bool

propAboveBeside nw ne sw se =
  (nw `beside` ne) `above` (sw `beside` se) 
  == 
  (nw `above` sw) `beside` (ne `above` se) 

propAboveBeside3Correct :: Picture -> Picture -> Property

propAboveBeside3Correct w e =
  (rectangular w && rectangular e && height w == height e) 
  ==>
     (w `beside` e) `above` (w `beside` e) 
         == 
     (w `above` w) `beside` (e `above` e) 

rectangular :: Picture -> Bool

rectangular = error "for you to define"

height :: Picture -> Int

height = error "for you to define"

-- Extended exercise: positioned pictures
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Positions on a plane.

type Position = (Int,Int)

-- An Image is a picture with a position.

type Image = (Picture,Position)

-- makeImage :: Picture -> Position -> Image
-- changePosition :: Image -> Position -> Image
-- moveImage :: Image -> Int -> Int -> Image
-- printImage :: Image -> IO ()


-- Local definitions
-- ^^^^^^^^^^^^^^^^^

-- The sum of the squares of two numbers.  

sumSquares :: Integer -> Integer -> Integer

sumSquares n m 
  = sqN + sqM
    where
    sqN = n*n
    sqM = m*m

-- Add corresponding elements in two lists; lists truncated to the length of the
-- shorter one.

addPairwise :: [Integer] -> [Integer] -> [Integer]
addPairwise intList1 intList2
  = [ m + n | (m,n) <- zip intList1 intList2 ]

-- A variant of addPairwise which doesn't truncate; see book for details of how
-- it works.

addPairwise' :: [Integer] -> [Integer] -> [Integer]

addPairwise' intList1 intList2
  = front ++ rear
    where
    minLength = min (length intList1) (length intList2)
    front     = addPairwise (take minLength intList1) 
                            (take minLength intList2)
    rear      = drop minLength intList1 ++ drop minLength intList2

-- and a variant of this ...

addPairwise'' :: [Integer] -> [Integer] -> [Integer]

addPairwise'' intList1 intList2
  = front ++ rear
    where
    minLength      = min (length intList1) (length intList2)
    front          = addPairwise front1 front2
    rear           = rear1 ++ rear2
    (front1,rear1) = splitAt minLength intList1
    (front2,rear2) = splitAt minLength intList2



-- Extended exercise: supermarket billing
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Types of names, prices (pence) and bar-codes.

type Name    = String
type Price   = Int
type BarCode = Int

-- The database linking names prices and bar codes.

type Database = [ (BarCode,Name,Price) ]

-- The example database we use is

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

-- The lists of bar codes, and of Name,Price pairs.

type TillType = [BarCode]
type BillType = [(Name,Price)]

-- The length of a line in the bill.

lineLength :: Int
lineLength = 30


