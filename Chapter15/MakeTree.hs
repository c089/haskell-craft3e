-------------------------------------------------------------------------
-- 								
--         MakeTree.hs							
-- 								
--         Turn a frequency table into a Huffman tree			
-- 								
--         (c) Addison-Wesley, 1996-2011.					
-- 							
-------------------------------------------------------------------------

module MakeTree ( makeTree ) where

import Types ( Tree(Leaf,Node), Bit(L,R), HCode, Table )

-- Convert the trees to a list, then amalgamate into a single	
-- tree.								

makeTree :: [ (Char,Int) ] -> Tree

makeTree = makeCodes . toTreeList

-- Huffman codes are created bottom up: look for the least		
-- two frequent letters, make these a new "isAlpha" (i.e. tree)	
-- and repeat until one tree formed.				

-- The function toTreeList makes the initial data structure.		

toTreeList :: [ (Char,Int) ] -> [ Tree ]

toTreeList = map (uncurry Leaf)

-- The value of a tree.						

value :: Tree -> Int

value (Leaf _ n)   = n
value (Node n _ _) = n

-- Pair two trees.							

pair :: Tree -> Tree -> Tree

pair t1 t2 = Node (v1+v2) t1 t2
             where
             v1 = value t1
             v2 = value t2

-- Insert a tree in a list of trees sorted by ascending value.	

insTree :: Tree -> [Tree] -> [Tree]

insTree t [] = [t]
insTree t (t1:ts) 
  | (value t <= value t1)    = t:t1:ts
  | otherwise                = t1 : insTree t ts
-- 	
-- Amalgamate the front two elements of the list of trees.		

amalgamate :: [ Tree ] -> [ Tree ]

amalgamate ( t1 : t2 : ts )
  = insTree (pair t1 t2) ts

-- Make codes: amalgamate the whole list.				

makeCodes :: [Tree] -> Tree

makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts) 

