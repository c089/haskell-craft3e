-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	PerformanceIA.hs
--
-----------------------------------------------------------------------

module Main where

-- main = putStrLn (show (sumI 1 1000000))
main = putStrLn (show (sumIA 1 1000000))
--- main = putStrLn (show (sumIS 1 1000000))

sumI :: Integer -> Integer -> Integer

sumI n m
 | n>m       = 0
 | otherwise = n + sumI (n+1) m

sumIA :: Integer -> Integer -> Integer

sumIA n m = accIA n m 0

accIA n m s
 | n>m       = s
 | otherwise = accIA (n+1) m (n+s)

sumIS :: Integer -> Integer -> Integer

sumIS n m = accIS n m 0

accIS n m s
 | n>m       = s
 | otherwise = accIS (n+1) m $! (n+s)
