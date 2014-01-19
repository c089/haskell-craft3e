import Test.HUnit
import Test.QuickCheck
import Chapter5 (digits)
import Chapter7 (firstDigit)

-------------------------------------------------------------------------------
-- Exercise 7.1
-------------------------------------------------------------------------------

firstPlusOne :: [Integer] -> Integer
firstPlusOne []     = 0
firstPlusOne (x:xs) = x+1

testFirstPlusOne = TestList
    [ TestCase (assertEqual "" 0 (firstPlusOne []))
    , TestCase (assertEqual "" 5 (firstPlusOne [4]))
    ]

-------------------------------------------------------------------------------
-- Exercise 7.2
-------------------------------------------------------------------------------

addFirst :: [Integer] -> Integer
addFirst [] = 0
addFirst (x:[]) = x
addFirst (x1:x2:xs) = x1+x2

testAddFirst = TestList
    [ TestCase (assertEqual "" 5 (addFirst [2,3]))
    , TestCase (assertEqual "" 2 (addFirst [2]))
    , TestCase (assertEqual "" 0 (addFirst []))
    ]


-------------------------------------------------------------------------------
-- Exercise 7.3
-------------------------------------------------------------------------------

firstPlusOne' :: [Integer] -> Integer
firstPlusOne' xs
  | xs == []    = 0
  | otherwise   = head xs + 1

addFirst' :: [Integer] -> Integer
addFirst' xs
    | l == 0    = 0
    | l == 1    = xs!!0
    | otherwise = xs!!0 + xs!!1
    where l = length xs

propFirstPlusOne x = firstPlusOne x == firstPlusOne' x
propAddFirst x = addFirst x == addFirst' x


-------------------------------------------------------------------------------
-- Exercise 7.4
-------------------------------------------------------------------------------
firstDigit' :: String -> Char
firstDigit' x = first (digits x)
    where
        first :: String -> Char
        first []    = '\0'
        first (x:_) = x

propFirstDigit x = firstDigit x == firstDigit' x

-------------------------------------------------------------------------------
-- Exercise 7.5
-------------------------------------------------------------------------------

listProduct :: [Integer] -> Integer
listProduct []      = 1
listProduct (x:xs)  = x * listProduct xs

testProduct = TestCase (assertEqual "3*4*5=60" 60 (listProduct [3,4,5]))

-- 1 chosen for [] because the recursion always expands to include
-- (listProduct []) and with a starting value of 0 that would always
-- result in 0


-------------------------------------------------------------------------------
-- Exercise 7.6
-------------------------------------------------------------------------------

and', or' :: [Bool] -> Bool

and' []     = True
and' (x:xs) = x && (and xs)

or' []      = False
or' (x:xs)  = x || (or xs)

testAndOr = TestList
    [ TestCase (assertEqual "" False (and' [False, False]))
    , TestCase (assertEqual "" False (and' [False, True]))
    , TestCase (assertEqual "" False (and' [True, False]))
    , TestCase (assertEqual "" True  (and' [True, True]))
    , TestCase (assertEqual "" False (or'  [False, False]))
    , TestCase (assertEqual "" True  (or'  [False, True]))
    , TestCase (assertEqual "" True  (or'  [True, False]))
    , TestCase (assertEqual "" True  (or'  [True, True]))
    ]

propAnd x = and x == and' x
propOr  x = or  x == or'  x

-- Again, the recursion always includes the value of and/or []. If and []
-- returned False, the recursion could never result in True.

-------------------------------------------------------------------------------
-- Exercise 7.8
-------------------------------------------------------------------------------

elemNum :: Integer -> [Integer] -> Integer
elemNum _ []     = 0
elemNum a (x:xs)
    | a == x    = 1 + elemNum a xs
    | otherwise = elemNum a xs

testElemNum = TestList
    [ TestCase (assertEqual "" 0 (elemNum 5 []))
    , TestCase (assertEqual "" 0 (elemNum 5 [1,2,3]))
    ]
