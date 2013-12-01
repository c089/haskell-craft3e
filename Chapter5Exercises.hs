module Chapter5Exercises where
import Chapter4 hiding (maxThree)
import Chapter4Exercises
import Chapter5
import Test.HUnit
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Exercise 5.1
-------------------------------------------------------------------------------

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b
    | a == b = (m, 2)
    | otherwise = (m, 1)
    where m = max a b

maxThreeOccurs' :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs' a b c
    | max_ab < c    = (c, 1)
    | max_ab == c   = (c, occ_ab + 1)
    | max_ab > c    = (max_ab, occ_ab)
    where max_ab    = fst m
          occ_ab    = snd m
          m         = maxOccurs a b

testMaxThreeOccurs' = TestList
    [ TestCase (assertEqual "max a b == c"  (1,2) (maxThreeOccurs' 0 1 1)),
      TestCase (assertEqual "max a b < c"   (2,1) (maxThreeOccurs' 0 1 2)),
      TestCase (assertEqual "max a b > c"   (2,1) (maxThreeOccurs' 2 1 0)),
      TestCase (assertEqual "a==b==c"       (0,3) (maxThreeOccurs' 0 0 0))
    ]

prop_maxThreeOccurs a b c = maxThreeOccurs a b c == maxThreeOccurs' a b c


-------------------------------------------------------------------------------
-- Exercise 5.2
-------------------------------------------------------------------------------

minThree a b c = a `min` b `min` c

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (a,b,c) = ((minThree a b c), (middleNumber a b c), (maxThree a b c))

testOrderTriple = TestList
    [ TestCase (assertEqual "already sorted" (0,1,2) (orderTriple (0,1,2)))
    , TestCase (assertEqual "all same" (0,0,0) (orderTriple (0,0,0)))
    , TestCase (assertEqual "reverse order" (0,1,2) (orderTriple (2,1,0)))
    ]

-------------------------------------------------------------------------------
-- Exercise 5.5
-------------------------------------------------------------------------------

perimeter :: Shape -> Float
perimeter (Circle r) = pi*2*r


-------------------------------------------------------------------------------
-- Exercise 5.6
-------------------------------------------------------------------------------

data DataItems = ShopItem' ProductName Quantity
type ProductName = String
type Quantity = Integer


-------------------------------------------------------------------------------
-- Exercise 5.18
-------------------------------------------------------------------------------

doubleAll :: [Integer] -> [Integer]
doubleAll l = [ 2*e | e <- l ]

testDoubleAll = TestCase (assertEqual "" [2,10,20] (doubleAll [1,5,10]))

-------------------------------------------------------------------------------
-- Exercise 5.19
-------------------------------------------------------------------------------

capitalize :: String -> String
capitalize s = [ capitalizeChar c | c <- s ]

capitalizeChar :: Char -> Char
capitalizeChar 'a' = 'A'
capitalizeChar 'A' = 'A'
capitalizeChar 'b' = 'B'
capitalizeChar 'B' = 'B'
capitalizeChar 'c' = 'C'
capitalizeChar 'C' = 'C'
capitalizeChar 'd' = 'D'
capitalizeChar 'D' = 'D'
capitalizeChar 'e' = 'E'
capitalizeChar 'E' = 'E'
capitalizeChar x  = x
-- and so on, or use Data.Char.isUpper but I thought that might be cheating

testCapitalize = TestList
    [ TestCase (assertEqual "all lower" "A B C" (capitalize "a b c"))
    , TestCase (assertEqual "all upper" "A B C" (capitalize "A B C"))
    , TestCase (assertEqual "mixed" "ABCDE" (capitalize "aBcdE"))
    ]

capitalizeLetters :: String -> String
capitalizeLetters s = [ capitalizeChar c | c <- s, isLetter c ]

isLetter :: Char -> Bool
isLetter x = capitalizeChar x `elem` "ABCDE"

testCapitalizeLetters = TestCase (assertEqual "" "ABC" (capitalizeLetters "a b c"))
