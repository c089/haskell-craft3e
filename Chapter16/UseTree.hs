-------------------------------------------------------------------------
--  
--         UseTree.hs
--  
-- 	   Using the search tree ADT					
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------
			


module UseTree where

import Tree					
--            
-- The size function  definable using the operations of the	
--  	abstype.							
--  

size :: Tree a -> Integer
size t 
  | isNil t 	= 0
  | otherwise 	= 1 + size (leftSub t) + size (rightSub t)

--  
-- Finding the nth element of a tree.				
--  

indexT :: Integer -> Tree a -> a

indexT n t 
  | isNil t 	= error "indexT"
  | n < st1 	= indexT n t1
  | n == st1 	= v
  | otherwise 	= indexT (n-st1-1) t2
      where
      v   = treeVal t
      t1  = leftSub t
      t2  = rightSub t
      st1 = size t1

