-------------------------------------------------------------------------
-- 								
--         Frequency.hs							
-- 								
--         Calculating the frequencies of words in a text, used in 	
--         Huffman coding.							
-- 								
--         (c) Addison-Wesley, 1996-2011.					
-- 								
-------------------------------------------------------------------------

module Frequency ( frequency ) where

import Test.QuickCheck hiding ( frequency )

-- Calculate the frequencies of characters in a list.		
-- 								
-- This is done by sorting, then counting the number of		
-- repetitions. The counting is made part of the merge 		
-- operation in a merge sort.					

frequency :: [Char] -> [ (Char,Int) ]

frequency
  = mergeSort freqMerge . mergeSort alphaMerge . map start
    where
    start ch = (ch,1)

-- Merge sort parametrised on the merge operation. This is more	
-- general than parametrising on the ordering operation, since	
-- it permits amalgamation of elements with equal keys		
-- for instance.							
--  
mergeSort :: ([a]->[a]->[a]) -> [a] -> [a]

mergeSort merge xs
  | length xs < 2 	= xs					
  | otherwise		
      = merge (mergeSort merge first)
              (mergeSort merge second)	
        where
        first  = take half xs
        second = drop half xs
        half   = (length xs) `div` 2

-- Order on first entry of pairs, with				
-- accumulation of the numeric entries when equal first entry.

alphaMerge :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]	

alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
  | (p==q) 	= (p,n+m) : alphaMerge xs ys		
  | (p<q) 	= (p,n) : alphaMerge xs ((q,m):ys)	
  | otherwise 	= (q,m) : alphaMerge ((p,n):xs) ys	

-- Lexicographic ordering, second field more significant.
-- 		
freqMerge :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]	

freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
  | (n<m || (n==m && p<q)) 
    = (p,n) : freqMerge xs ((q,m):ys)	
  | otherwise 
    = (q,m) : freqMerge ((p,n):xs) ys	

-- QuickCheck property

prop_mergeSort :: [Int] -> Bool

prop_mergeSort xs =
    sorted (mergeSort merge xs) 
           where
             sorted [] = True
             sorted [_] = True
             sorted (x:y:ys) = x<=y && sorted (y:ys)

             merge [] xs = xs
             merge ys [] = ys
             merge (x:xs) (y:ys) 
                  | x<=y      = x: merge xs (y:ys)
                  | otherwise = y: merge (x:xs) ys