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

-------------------------------------------------------------------------------
-- Exercise 5.20
-------------------------------------------------------------------------------

divides x y = x `mod` y == 0
divisors :: Integer -> [Integer]
divisors x = [ y | y <- [1..x], divides x y ]

testDivisors = TestList
    [ TestCase (assertEqual "non-positive"      []              (divisors 0))
    , TestCase (assertEqual "divisors of 12"    [1,2,3,4,6,12]  (divisors 12))
    , TestCase (assertEqual "prime"             [1, 13]         (divisors 13))
    ]


isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

testIsPrime = TestList
    [ TestCase (assertEqual "1" False (isPrime 1))
    , TestCase (assertEqual "2" True (isPrime 2))
    , TestCase (assertEqual "3" True (isPrime 3))
    , TestCase (assertEqual "4" False (isPrime 4))
    , TestCase (assertEqual "5" True (isPrime 5))
    ]


-------------------------------------------------------------------------------
-- Exercise 5.21
-------------------------------------------------------------------------------

matches :: Integer -> [Integer] -> [Integer]
matches toMatch list = [ e | e<-list, e == toMatch ]

testMatches = TestCase (assertEqual "5s" [5,5] (matches 5 [1,2,3,4,5,0,5,1] ))

elem' :: Integer -> [Integer] -> Bool
elem' element list = length (matches element list) > 0

testElem' = TestList
    [ TestCase (assertEqual "nope" False (elem' 5 [1,2,3,4]))
    , TestCase (assertEqual "yeah" True  (elem' 5 [1,2,3,4,5]))
    , TestCase (assertEqual "twice" True  (elem' 5 [5,5]))
    ]

prop_elem'eqelem a b = elem a b == elem' a b

-------------------------------------------------------------------------------
-- Exercise 5.22
-------------------------------------------------------------------------------

joinStrings :: [String] -> String
joinStrings ss = [ c | s <- ss, c <- s ]

onSeparateLines :: [String] -> String
onSeparateLines ls = joinStrings [ l++"\n" | l <- ls  ]

testOnSeparateLines = TestCase (
    assertEqual "should concat strings with newline character"
    "foo\nbar\nbaz\n" (onSeparateLines ["foo", "bar", "baz"]))

-------------------------------------------------------------------------------
-- Exercise 5.23
-------------------------------------------------------------------------------

duplicate :: String -> Integer -> String
duplicate s n = joinStrings [ s | x <- [0..n-1] ]

testDuplicate = TestList
    [ TestCase (assertEqual "empty for n = 0" "" (duplicate "foo" 0))
    , TestCase (assertEqual "string for n = 1" "foo" (duplicate "foo" 1))
    , TestCase (assertEqual "duplicated for n>1" "foofoo" (duplicate "foo" 2))
    , TestCase (assertEqual "more dupes" "ababababab" (duplicate "ab" 5))
    ]


-------------------------------------------------------------------------------
-- Exercise 5.24
-------------------------------------------------------------------------------

pushRight :: String -> String
pushRight s = padding ++ s
    where padding = [ ' ' | _ <- [1..n] ]
          n          = linelength - length s
          linelength = 12

testPushRight = TestList
    [ TestCase (assertEqual "single char" "           x" (pushRight "x") )
    , TestCase (assertEqual "croc" "   crocodile" (pushRight "crocodile"))
    ]
