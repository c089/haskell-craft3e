import Chapter4 hiding (maxThree)
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Exercise 4.1
-------------------------------------------------------------------------------

-- Copied from Chapter4.hs to allow using Integer
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = (x `max` y) `max` z

-- Using maxThree, but not max
maxFourA :: Integer -> Integer -> Integer -> Integer -> Integer
maxFourA a b c d
    | (maxThree a b c) >= d   = maxThree a b c
    | otherwise               = d

-- Using max only
maxFourB :: Integer -> Integer -> Integer -> Integer -> Integer
maxFourB a b c d = max (max a b) (max c d)

-- Using maxThree and max
maxFourC :: Integer -> Integer -> Integer -> Integer -> Integer
maxFourC a b c d = (maxThree a b c) `max` d

maxFour = maxFourC

-- All three should yield the same result
prop_maxFour :: Integer -> Integer -> Integer -> Integer -> Bool
prop_maxFour a b c d =
    maxFourA a b c d == maxFourB a b c d &&
    maxFourB a b c d == maxFourC a b c d

-------------------------------------------------------------------------------
-- Exercise 4.2
-------------------------------------------------------------------------------

-- Implementation of between had to be done in Chapter4.hs

-------------------------------------------------------------------------------
-- Exercise 4.3
-------------------------------------------------------------------------------

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual a b c
    | a == b && b == c = 3
    | a == b           = 2
    | a == c           = 2
    | b == c           = 2
    | otherwise        = 0

-------------------------------------------------------------------------------
-- Exercise 4.4
-------------------------------------------------------------------------------
howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEqual a b c d
    | a == b && b == c && c == d    = 4
    | otherwise                     = maxFour (howManyEqual a b c)
                                              (howManyEqual a b d)
                                              (howManyEqual a c d)
                                              (howManyEqual b c d)
