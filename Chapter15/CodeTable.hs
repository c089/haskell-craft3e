-------------------------------------------------------------------------
--  
--         CodeTable.hs							
-- 								
--         Converting a Huffman tree to a ord table.			
-- 								
--         (c) Addison-Wesley, 1996-2011.					
-- 								
-------------------------------------------------------------------------

module CodeTable ( codeTable ) where

import Types ( Tree(Leaf,Node), Bit(L,R), HCode, Table )

-- Making a table from a Huffman tree.				

codeTable :: Tree -> Table

codeTable = convert []

-- Auxiliary function used in conversion to a table. The first argument is
-- the HCode which codes the path in the tree to the current Node, and so
-- codeTable is initialised with an empty such sequence.		

convert :: HCode -> Tree -> Table

convert cd (Leaf c n) =  [(c,cd)]
convert cd (Node n t1 t2)
	= (convert (cd++[L]) t1) ++ (convert (cd++[R]) t2)


-- Show functions						
-- ^^^^^^^^^^^^^^

-- Show a tree, using indentation to show structure.		
-- 								
showTree :: Tree -> String

showTree t = showTreeIndent 0 t

-- The auxiliary function showTreeIndent has a second, current 
-- level of indentation, as a parameter.							

showTreeIndent :: Int -> Tree -> String

showTreeIndent m (Leaf c n) 
  = spaces m ++ show c ++ " " ++ show n ++ "\n"
showTreeIndent m (Node n t1 t2)
  = showTreeIndent (m+4) t1 ++
    spaces m ++ "[" ++ show n ++ "]" ++ "\n" ++
    showTreeIndent (m+4) t2

-- A String of n spaces.

spaces :: Int -> String

spaces n = replicate n ' '

-- To show a sequence of Bits. 					

showCode :: HCode -> String
showCode = map conv
	   where
	   conv R = 'R'
	   conv L = 'L'

-- To show a table of codes.

showTable :: Table -> String						
showTable 
  = concat . map showPair
    where
    showPair (ch,co) = [ch] ++ " " ++ showCode co ++ "\n"
