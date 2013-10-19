-----------------------------------------------------------------------
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2010.
--
-- 	Pictures.hs
-- 
--     An implementation of a type of rectangular pictures  
--     using lists of lists of characters. 
-----------------------------------------------------------------------



-- The basics
-- ^^^^^^^^^^

module Pictures where
import Test.QuickCheck


type Picture = [[Char]]

-- The example used in Craft2e: a polygon which looks like a horse. Here
-- taken to be a 16 by 12 rectangle.

horse :: Picture

horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]

-- Completely white and black pictures.

white :: Picture

white = ["......",
         "......",
         "......",
         "......",
         "......",
         "......"]

black = ["######",
         "######",
         "######",
         "######",
         "######",
         "######"]

-- Getting a picture onto the screen.

printPicture :: Picture -> IO ()

printPicture = putStr . concat . map (++"\n")


-- Transformations of pictures.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Reflection in a vertical mirror.

flipV :: Picture -> Picture

flipV = map reverse

-- Reflection in a horizontal mirror.

flipH :: Picture -> Picture

flipH = reverse

-- Rotation through 180 degrees, by composing vertical and horizontal
-- reflection. Note that it can also be done by flipV.flipH, and that we
-- can prove equality of the two functions.

rotate :: Picture -> Picture

rotate = flipH . flipV

-- One picture above another. To maintain the rectangular property,
-- the pictures need to have the same width.

above :: Picture -> Picture -> Picture

above = (++)

-- One picture next to another. To maintain the rectangular property,
-- the pictures need to have the same height.

beside :: Picture -> Picture -> Picture

beside = zipWith (++)

-- Superimose one picture above another. Assume the pictures to be the same
-- size. The individual characters are combined using the combine function.

superimpose :: Picture -> Picture -> Picture

superimpose = zipWith (zipWith combine)

-- For the result to be '.' both components have to the '.'; otherwise
-- get the '#' character.

combine :: Char -> Char -> Char

combine topCh bottomCh
  = if (topCh == '.' && bottomCh == '.') 
    then '.'
    else '#'

-- Inverting the colours in a picture; done pointwise by invert...

invertColour :: Picture -> Picture

invertColour = map (map invert)

-- ... which works by making the result '.' unless the input is '.'.

invert :: Char -> Char

invert ch = if ch == '.' then '#' else '.'


-- Property

prop_rotate, prop_flipV, prop_flipH :: Picture -> Bool

prop_rotate pic = flipV (flipH pic) == flipH (flipV pic)

prop_flipV pic = flipV (flipV pic) == pic

prop_flipH pic = flipH (flipV pic) == pic

test_rotate, test_flipV, test_flipH :: Bool
 
test_rotate = flipV (flipH horse) == flipH (flipV horse)

test_flipV = flipV (flipV horse) == horse

test_flipH = flipH (flipV horse) == horse

-- More properties

prop_AboveFlipV pic1 pic2 = 
    flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2) 

prop_AboveFlipH pic1 pic2 = flipH (pic1 `above` pic2) == (flipH pic2) `above` (flipH pic1)

propAboveBeside1 nw ne sw se =
  (nw `beside` ne) `above` (sw `beside` se) 
  == 
  (nw `above` sw) `beside` (ne `above` se) 

propAboveBeside2 n s =
  (n `beside` n) `above` (s `beside` s) == (n `above` s) `beside` (n `above` s) 

propAboveBeside3 w e =
  (w `beside` e) `above` (w `beside` e) == (w `above` w) `beside` (e `above` e) 

propAboveBeside3Correct w e =
  (rectangular w && rectangular e && height w == height e) 
  ==>
     (w `beside` e) `above` (w `beside` e) 
         == 
     (w `above` w) `beside` (e `above` e) 

-- auxiliary properties and functions

notEmpty pic = pic /= []

rectangular pic =
  notEmpty pic &&
  and [ length first == length l | l <-rest ]
  where
    (first:rest) = pic

height, width :: Picture -> Int

height = length
width = length . head

size :: Picture -> (Int,Int)

size pic = (width pic, height pic)

propAboveBesideFull nw ne sw se =
  (rectangular nw && rectangular ne && rectangular sw && rectangular se &&
   size nw == size ne && size ne == size se && size se == size sw) ==>
  (nw `beside` ne) `above` (sw `beside` se) == (nw `above` sw) `beside` (ne `above` se) 

-- Using explicit generators ...


prop_1 = forAll (choose (1,10)) $ \x -> x/=x+(x::Int)

prop_2 = forAll (choose (1,10)) $ \x -> x/=(x::Int)

-- Generators suited to Pictures

-- chose either '.' or '#'

genChar :: Gen Char

genChar = oneof [return '.', return '#']

-- generate a list of length n each element from generator g.

genList :: Int -> Gen a -> Gen [a]

genList n g = sequence [ g | i<-[1..n] ]

-- generate a picture of given size using '.' and '#'

genSizedPicture :: Int -> Int -> Gen [String]

genSizedPicture height width =
      sequence [ genList width genChar | i<-[1::Int .. height] ]

-- generate a picture of random size using '.' and '#'

genPicture :: Gen [String]

genPicture =
    do
      height <- choose (1,10)
      width  <- choose (1,10)
      genSizedPicture height width

-- generate four pictures of the *same* random size using '.' and '#'

genFourPictures :: Gen ([String],[String],[String],[String])

genFourPictures =
    do
      height <- choose (1,10)
      width  <- choose (1,10)
      nw <- genSizedPicture height width
      ne <- genSizedPicture height width
      sw <- genSizedPicture height width
      se <- genSizedPicture height width
      return (nw,ne,sw,se)

-- test that above and besides commute when used with four pictures
-- of the same size

prop_AboveBeside =
    forAll genFourPictures $ \(nw,ne,sw,se) -> propAboveBeside1 nw ne sw se