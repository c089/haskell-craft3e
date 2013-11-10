import Chapter4 hiding (maxThree)
import Test.HUnit
import Test.QuickCheck hiding (Result)

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

-------------------------------------------------------------------------------
-- Exercise 4.8
-------------------------------------------------------------------------------

triArea'' :: Float -> Float -> Float -> Float
triArea'' a b c
    | possible   = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise  = 0
    where
      s                                 = (a+b+c)/2
      possible                          = allPositive &&
                                          allSatisfyTriangleInequality
      allPositive                       = a > 0 && b > 0 && c > 0
      allSatisfyTriangleInequality      = satisfyTriangleInequality a b c &&
                                          satisfyTriangleInequality b a c &&
                                          satisfyTriangleInequality c a b
      satisfyTriangleInequality a b c   = a < (b + c)

-------------------------------------------------------------------------------
-- Exercise 4.9
-------------------------------------------------------------------------------

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c = (maxValue, occurences)
    where
      maxValue      = maxThree a b c
      occurences    = occurrencesOf maxValue
      occurrencesOf n
        | a == n && b == n && c == n   = 3
        | a == n && b == n             = 2
        | a == n && c == n             = 2
        | b == n && c == n             = 2
        | otherwise                    = 1

-------------------------------------------------------------------------------
-- Exercise 4.11, 4.12, 4.13
-------------------------------------------------------------------------------

data Result = Win | Lose | Draw deriving (Show, Eq)

outcome :: Move -> Move -> Result
outcome a b
    | a == beat b   = Win
    | a == lose b   = Lose
    | otherwise     = Draw

testRPS = TestList [
    TestCase (assertEqual "rock beats scissors"  Win (outcome Rock Scissors)),
    TestCase (assertEqual "paper beats rock"  Win (outcome Paper Rock)),
    TestCase (assertEqual "scissors beats paper"  Win (outcome Scissors Paper)),
    TestCase (assertEqual "scissors loses to rock" Lose (outcome Scissors Rock)),
    TestCase (assertEqual "rock loses to paper" Lose (outcome Rock Paper)),
    TestCase (assertEqual "paper loses to scissors" Lose (outcome Paper Scissors)),
    TestCase (assertEqual "draw Scissors" Draw (outcome Scissors Scissors)),
    TestCase (assertEqual "draw Paper" Draw (outcome Paper Paper)),
    TestCase (assertEqual "draw Rock" Draw (outcome Rock Rock))
 ]

propCannotBeatAndLoseAgainstTheSame a = beat a /= lose a


-------------------------------------------------------------------------------
-- Exercise 4.15, 4.16
-------------------------------------------------------------------------------

data Temp = Cold | Hot deriving (Eq, Show, Ord)
data Season = Spring | Summer | Autumn | Winter deriving (Eq, Show, Ord)

temperatureIn :: Season -> Temp
temperatureIn Spring = Cold
temperatureIn Summer = Hot
temperatureIn Autumn = Cold
temperatureIn Winter = Cold

data Month = January | February | March | April | May | June |
             July | August | September | October | November| December 
     deriving (Show, Eq, Ord)

seasonIn :: Month -> Season
seasonIn month
    | month <= March        = Spring
    | month <= August       = Summer
    | month <= September    = Autumn
    | otherwise             = Winter

-------------------------------------------------------------------------------
-- Exercise 4.17
-------------------------------------------------------------------------------

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
    | n < m     = 0
    | m == n    = n
    | otherwise = (rangeProduct m (n-1)) * n

testRangeProduct = TestList
    [ TestCase (assertEqual "for m > n"  0 (rangeProduct 2 1))
    , TestCase (assertEqual "for m=n=1"  1 (rangeProduct 1 1))
    , TestCase (assertEqual "for m=1,n=2" 2 (rangeProduct 1 2))
    , TestCase (assertEqual "for m=1,n=3" 6 (rangeProduct 1 3))
    , TestCase (assertEqual "for m=1,n=4" 24 (rangeProduct 1 4))
    , TestCase (assertEqual "for m=4,n=4" 4 (rangeProduct 4 4))
    , TestCase (assertEqual "for m=4,n=5" 20 (rangeProduct 4 5))
    ]

-------------------------------------------------------------------------------
-- Exercise 4.18
-------------------------------------------------------------------------------

fac' :: Integer -> Integer
fac' n
    | n < 0   = error "fac only defined on natural numbers"
    | n == 0    = 1
    | otherwise = rangeProduct 1 n

propFac'ShouldBeSameAsFac n
    | n >= 0    = fac n == fac' n
    | otherwise = True

-------------------------------------------------------------------------------
-- Exercise 4.19
-------------------------------------------------------------------------------
multiplyUsingAdd a b
    | a == 0    = 0
    | otherwise = multiplyUsingAdd (a-1) b + b

testMultiplyUsingAddition = TestList
    [ TestCase (assertEqual "0 * 2" 0  (multiplyUsingAdd 0 2))
    , TestCase (assertEqual "2 * 0" 0  (multiplyUsingAdd 2 0))
    , TestCase (assertEqual "1 * 2" 2  (multiplyUsingAdd 1 2))
    , TestCase (assertEqual "2 * 1" 2  (multiplyUsingAdd 2 1))
    , TestCase (assertEqual "3 * 1" 3  (multiplyUsingAdd 3 1))
    , TestCase (assertEqual "1 * 3" 3  (multiplyUsingAdd 1 3))
    , TestCase (assertEqual "2 * 2" 4  (multiplyUsingAdd 2 2))
    , TestCase (assertEqual "7 * 9" 63 (multiplyUsingAdd 7 9))
    ]

propMultiplyUsingAddShouldEqualMul a b
    | a >= 0 && b >= 0  = multiplyUsingAdd a b == a * b
    | otherwise         = True


-------------------------------------------------------------------------------
-- Exercise 4.20
-------------------------------------------------------------------------------
integerSquareRoot :: Integer -> Integer
integerSquareRoot n = isrInternal n n
    where isrInternal n m
            | n*n <= m  = n
            | otherwise = isrInternal (n-1) m

testIntegerSquareRoot = TestList
    [ TestCase (assertEqual "4"     2 (integerSquareRoot 4))
    , TestCase (assertEqual "15"    3 (integerSquareRoot 15))
    , TestCase (assertEqual "16"    4 (integerSquareRoot 16))
    ]
