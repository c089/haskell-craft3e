-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Index
--
-----------------------------------------------------------------------



module Index where

import Chapter11 ((>.>))
import qualified Chapter7



-- Example: creating an index
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The basic type symonyms

type Doc  = String
type Line = String
type Word = String

-- The type of the top-level function

makeIndex :: Doc -> [ ([Int],Word) ]

-- The top-level definition

makeIndex
  = lines       >.>     --   Doc            -> [Line]
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs      >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten             --   [([Int],Word)] -> [([Int],Word)]

-- Implementing the component functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
-- Attach a number to each line.

numLines :: [Line] -> [ ( Int , Line ) ]
numLines linels
  = zip [1 .. length linels] linels

-- Associate each word with a line number

numWords :: ( Int , Line ) -> [ ( Int , Word ) ]

numWords (number , line)
  = [ (number , word) | word <- Chapter7.splitWords line ]

-- The definition uses splitWords from Chapter 7, modified to use a different
-- version of whitespace. For this to take effect, need to make the modification
-- in the Chapter7.hs file.

whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

-- Apply numWords to each integer,line pair.

allNumWords :: [ ( Int , Line ) ] -> [ ( Int , Word ) ]
allNumWords = concat . map numWords

-- The list must next be
-- sorted by word order, and lists of lines on which a word appears be built.
-- The ordering relation on pairs of numbers and 
-- words is given by

orderPair :: ( Int , Word ) -> ( Int , Word ) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 )
  = w1 < w2 || ( w1 == w2 && n1 < n2 )

-- Sorting the list using the orderPair ordering on pairs.

sortLs :: [ ( Int , Word ) ] -> [ ( Int , Word ) ]

sortLs []     = []
sortLs (p:ps)
  = sortLs smaller ++ [p] ++ sortLs larger
    where
    smaller = [ q | q<-ps , orderPair q p ]
    larger  = [ q | q<-ps , orderPair p q ]

-- The entries for the same word need to be accumulated together.
-- First each entry is converted to having a list of line numbers associated with
-- it, thus

makeLists ::  [ (Int,Word) ] -> [ ([Int],Word) ]
makeLists 
  = map mklis 
    where
    mklis ( n , st ) = ( [n] , st )

-- After this, the lists associated with the same words are amalgamated.

amalgamate :: [ ([Int],Word) ] -> [ ([Int],Word) ]

amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | w1 /= w2    = (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise   = amalgamate ((l1++l2,w1):rest)

-- Remove all the short words.

shorten :: [([Int],Word)] -> [([Int],Word)]

shorten 
  = filter sizer 
    where
    sizer (nl,wd) = length wd > 3