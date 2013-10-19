-------------------------------------------------------------------------
-- 								
--         MakeCode.hs							
-- 								
--         Huffman coding in Haskell.					
-- 								
--         (c) Addison-Wesley, 1996-2011.					
-- 							
-------------------------------------------------------------------------

module MakeCode ( codes, codeTable ) where

import Types
import Frequency ( frequency )
import MakeTree  ( makeTree )
import CodeTable ( codeTable )

-- Putting together frequency calculation and tree conversion	

codes :: [Char] -> Tree

codes = makeTree . frequency

