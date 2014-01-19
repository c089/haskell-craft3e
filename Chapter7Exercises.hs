import Test.HUnit
import Test.QuickCheck

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

