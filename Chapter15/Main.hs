-------------------------------------------------------------------------
--
--         Main.hs
--
-- 	The main module of the Huffman example
--
-- 	(c) Addison-Wesley, 1996-2011.
--
-------------------------------------------------------------------------

-- The main module of the Huffman example

module Main (main, codeMessage, decodeMessage, codes, codeTable ) where

import Types    ( Tree(Leaf,Node), Bit(L,R), HCode , Table )
import Coding   ( codeMessage, decodeMessage ) 
import MakeCode ( codes, codeTable )


main = print decoded


-- Examples
-- ^^^^^^^^

-- The coding table generated from the text "there is a green hill".							

tableEx :: Table
tableEx = codeTable (codes "there is a green hill")

-- The Huffman tree generated from the text "there is a green hill",
-- from which tableEx is produced by applying codeTable.

treeEx :: Tree
treeEx = codes "there is a green hill"

-- A message to be coded.

message :: String
message = "there are green hills here"

-- The message in code.

coded :: HCode
coded = codeMessage tableEx message

-- The coded message decoded.

decoded :: String
decoded = decodeMessage treeEx coded
