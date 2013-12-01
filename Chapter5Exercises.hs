import Chapter4Exercises
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
