-------------------------------------------------------------------------
-- 
--         Relation.hs				
--
--         Building Relations and Graphs on top of the Set ADT.			
--  
--         (c) Addison-Welsey, 1996-2011.					
--        
---------------------------------------------------------------------------
				
                                                                       
module Relation where

import Set
import Data.List hiding ( union )
--  
-- A relation is a set of pairs.					

type Relation a = Set (a,a)
--  

-- Operations over relations.					
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^ 
--  
-- The image of an element under a relation.			

image :: Ord a => Relation a -> a -> Set a

image rel val = mapSet snd (filterSet ((==val).fst) rel)
--  
-- The image of a set of elements under a relation.		
--  
setImage :: Ord a => Relation a -> Set a -> Set a

setImage rel = unionSet . mapSet (image rel) 

-- The union of a set of sets.					
--  
unionSet :: Ord a => Set (Set a) -> Set a

unionSet = foldSet union empty
--  
-- Add to a set its image under a relation.			

addImage :: Ord a => Relation a -> Set a -> Set a

addImage rel st = st `union` setImage rel st
--  
-- Add the children (under the relation isParent) to a set.	
--  
type People = String

isParent :: Relation People

isParent = isParent   --  dummy definition
                      --  needs to be replaced

addChildren :: Set People -> Set People

addChildren = addImage isParent 
--  
-- Compose two relations.						
--  
compose :: Ord a => Relation a -> Relation a -> Relation a

compose rel1 rel2
  =  mapSet outer (filterSet equals (setProduct rel1 rel2))
     where
     equals ((a,b),(c,d)) = (b==c)
     outer  ((a,b),(c,d)) = (a,d)

-- The product of two sets.					
--  
setProduct :: (Ord a,Ord b) => Set a -> Set b -> Set (a,b)

setProduct st1 st2 = unionSet (mapSet (adjoin st1) st2)
--  
-- Add an element to each element of a set, forming a set of pairs.
--  
adjoin :: (Ord a,Ord b) => Set a -> b -> Set (a,b)

adjoin st el = mapSet (addEl el) st
               where
               addEl el el' = (el',el)
--  
-- The transitive closure of a relation.				 

tClosure :: Ord a => Relation a -> Relation a

tClosure rel = limit addGen rel
               where
               addGen rel' = rel' `union` compose rel' rel

-- Finding a limit of a function.

limit             :: Eq a => (a -> a) -> a -> a
limit f xs 
  | xs == next 	        = xs
  | otherwise 		= limit f next
    where
    next = f xs

-- Graphs								
-- ^^^^^^ 
--  
-- The connected components of a graph.				 

connect :: Ord a => Relation a -> Relation a

connect rel = clos `inter` solc
              where
              clos = tClosure rel
              solc = inverse clos
--  
-- The inverse of a relation  swap all pairs.			 

inverse :: Ord a => Relation a -> Relation a

inverse = mapSet swap
          where 
          swap (x,y) = (y,x)
--  
-- The equivalence classes of a(n equivalence) relation.		
--  
classes :: Ord a => Relation a -> Set (Set a)

classes rel 
  = limit (addImages rel) start
    where
    start = mapSet sing (eles rel)

-- The auxiliary functions used in classes.			
--  
eles :: Ord a => Relation a -> Set a

eles rel = mapSet fst rel `union` mapSet snd rel

addImages :: Ord a => Relation a -> Set (Set a) -> Set (Set a)

addImages rel = mapSet (addImage rel)


-- Searching in graphs						
-- ^^^^^^^^^^^^^^^^^^^
--  
-- The descendants v under rel which lie outside st.		
--  
newDescs :: Ord a => Relation a -> Set a -> a -> Set a
newDescs rel st v = image rel v `diff` st
--  
-- Breaking the abstraction barrier for sets.			 

-- defined in Sets.hs
-- flatten :: Ord a => Set a -> [a]

-- Under the list implementation, we can use			
-- 	flatten = id
						
--  
-- A list of new descendants.					
--  
findDescs :: Ord a => Relation a -> [a] -> a -> [a]
findDescs rel xs v = flatten (newDescs rel (makeSet xs) v)


--  
-- Breadth first search.						
-- ^^^^^^^^^^^^^^^^^^^^^

breadthFirst :: Ord a => Relation a -> a -> [a]

breadthFirst rel val
	= limit step start
	  where
	  start = [val]
	  step xs = xs ++ nub (concat (map (findDescs rel xs) xs))

--  
-- Depth first search.						
-- ^^^^^^^^^^^^^^^^^^^^^

depthFirst :: Ord a => Relation a -> a -> [a]

depthSearch :: Ord a => Relation a -> a -> [a] -> [a]

depthFirst rel v = depthSearch rel v []

depthSearch rel v used
	= v : depthList rel (findDescs rel used' v) used'
	  where
	  used' = v:used

depthList :: Ord a => Relation a -> [a] -> [a] -> [a]

depthList rel [] used = [] 

depthList rel (val:rest) used
  = next ++ depthList rel rest (used++next)
    where
    next 
      | elem val used 	 = []
      | otherwise 	 = depthSearch rel val used

--  
-- From the exercises...						
--  
-- distance :: Eq a => Relation a -> a -> a -> Int



