-------------------------------------------------------------------------
--
--         Test.hs
--
-- 	The test module of the Huffman example
--
-- 	(c) Addison-Wesley, 1996-2011.
--
-------------------------------------------------------------------------

module Test where

-- The test module of the Huffman example

import Main
import Test.QuickCheck
import Data.List ( nub )


-- QuickCheck testing

checkInverse :: String -> Bool

checkInverse string = 
    decodeMessage tree (codeMessage table string) == string
        where
          tree = codes string
          table = codeTable tree

-- prop_Hufmann :: String -> Bool

prop_Hufmann string =
    (length (nub string) > 1) ==> checkInverse string
    
