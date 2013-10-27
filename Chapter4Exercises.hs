import Chapter4
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Exercise 4.1
-------------------------------------------------------------------------------

-- Using maxThree, but not max
maxFourA :: Int -> Int -> Int -> Int -> Int
maxFourA a b c d
    | (maxThree a b c) >= d   = maxThree a b c
    | otherwise               = d

-- Using max only
maxFourB :: Int -> Int -> Int -> Int -> Int
maxFourB a b c d = max (max a b) (max c d)

-- Using maxThree and max
maxFourC :: Int -> Int -> Int -> Int -> Int
maxFourC a b c d = (maxThree a b c) `max` d

-- All three should yield the same result
prop_maxFour :: Int -> Int -> Int -> Int -> Bool
prop_maxFour a b c d =
    maxFourA a b c d == maxFourB a b c d &&
    maxFourB a b c d == maxFourC a b c d
