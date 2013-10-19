-------------------------------------------------------------------------
-- 
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	Random number generation.
--
-------------------------------------------------------------------------

-- Lazy programming
-- ^^^^^^^^^^^^^^^^

module RandomGen where


-- Find the next (pseudo-)random number in the sequence.

nextRand :: Int -> Int
nextRand n = (multiplier*n + increment) `mod` modulus

-- A (pseudo-)random sequence is given by iterating this function,

randomSequence :: Int -> [Int]
randomSequence = iterate nextRand

-- Suitable values for the constants.

seed, multiplier, increment, modulus :: Int
seed       = 17489
multiplier = 25173
increment  = 13849
modulus    = 65536

-- Scaling the numbers to come in the (integer) range a to b (inclusive).

scaleSequence :: Int -> Int -> [Int] -> [Int]
scaleSequence s t
  = map scale
    where
    scale n = n `div` denom + s
    range   = t-s+1
    denom   = modulus `div` range

-- Turn a distribution into a function.

makeFunction :: [(a,Double)] -> (Double -> a)

makeFunction dist = makeFun dist 0.0

makeFun ((ob,p):dist) nLast rand
  | nNext >= rand && rand > nLast     
        = ob
  | otherwise                           
        = makeFun dist nNext rand
          where
          nNext = p*fromIntegral modulus + nLast

-- Random numbers from 1 to 6 according to the example distribution, dist.

randomTimes :: [Int]
randomTimes = map (makeFunction dist . fromIntegral) (randomSequence seed)

-- The distribution in question


dist :: [(Int,Double)]
dist = [(1,0.2), (2,0.25), (3,0.25), (4,0.15), (5,0.1), (6,0.05)]

