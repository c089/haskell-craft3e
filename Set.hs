-------------------------------------------------------------------------
-- 
--         Set.hs	
--
--         ADT of sets, implemented as ordered lists without repetitions.	
--  
--         (c) Addison-Welsey, 1996-2011.					
--        
---------------------------------------------------------------------------

module Set ( Set ,
  empty              , -- Set a
  sing               , -- a -> Set a
  memSet             , -- Ord a => Set a -> a -> Bool
  union,inter,diff   , -- Ord a => Set a -> Set a -> Set a
  eqSet              , -- Eq a  => Set a -> Set a -> Bool
  subSet             , -- Ord a => Set a -> Set a -> Bool
  makeSet            , -- Ord a => [a] -> Set a
  mapSet             , -- Ord b => (a -> b) -> Set a -> Set b
  filterSet          , -- (a -> Bool) -> Set a -> Set a
  foldSet            , -- (a -> a -> a) -> a -> Set a -> a
  showSet            , -- (a -> String) -> Set a -> String
  card               , -- Set a -> Int
  flatten              -- Set a -> [a]
  ) where

import Data.List hiding ( union )
--  
-- Instance declarations for Eq and Ord					

instance Eq a => Eq (Set a) where
  (==) = eqSet

instance Ord a => Ord (Set a) where
  (<=) = leqSet

-- The implementation.						
-- 				
newtype Set a = Set [a]

empty :: Set a
empty  = Set []

sing :: a -> Set a
sing x = Set [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (Set []) y    = False
memSet (Set (x:xs)) y 
  | x<y		= memSet (Set xs) y
  | x==y 	= True
  | otherwise 	= False

union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys        = ys
uni xs []        = xs
uni (x:xs) (y:ys) 
  | x<y 	= x : uni xs (y:ys)
  | x==y 	= x : uni xs ys
  | otherwise 	= y : uni (x:xs) ys

inter :: Ord a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys) 
  | x<y 	= int xs (y:ys)
  | x==y 	= x : int xs ys
  | otherwise 	= int (x:xs) ys

diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a]
dif [] ys = []
dif xs [] = xs
dif (x:xs) (y:ys)  
  | x<y 	= x : dif xs (y:ys)
  | x==y 	= dif xs ys
  | otherwise 	= dif (x:xs) ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys = True
subS xs [] = False
subS (x:xs) (y:ys) 
  | x<y 	= False
  | x==y 	= subS xs ys
  | x>y 	= subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys) = (xs == ys)

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = (xs <= ys)

--        	
makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
          where
          remDups []     = []
          remDups [x]    = [x]
          remDups (x:y:xs) 
	    | x < y 	= x : remDups (y:xs)
            | otherwise = remDups (y:xs)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (Set xs)  = (foldr f x xs)

showSet :: (a->String) -> Set a -> String
showSet f (Set xs) = concat (map ((++"\n") . f) xs)

card :: Set a -> Int
card (Set xs)     = length xs

-- Breaks the abstraction: used in Relation:

flatten (Set xs) = xs

