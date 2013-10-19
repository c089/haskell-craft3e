-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
--      CalcParseLib.hs
--
--      Library functions for parsing	
--      Note that this is not a monadic approach to parsing.
--
-----------------------------------------------------------------------
                     

module CalcParseLib where

import Data.Char

infixr 5 >*>
--   
-- The type of parsers.						
--  
type Parse a b = [a] -> [(b,[a])]
--  
-- Some basic parsers						
--  
--  
-- Fail on any input.						
--  
none :: Parse a b
none inp = []
--  
-- Succeed, returning the value supplied.				
--  
succeed :: b -> Parse a b 
succeed val inp = [(val,inp)]
--  
-- token t recognises t as the first value in the input.		
--  
token :: Eq a => a -> Parse a a
token t (x:xs) 
  | t==x 	= [(t,xs)]
  | otherwise 	= []
token t []    = []
--  
-- spot whether an element with a particular property is the 	
-- first element of input.						
--  
spot :: (a -> Bool) -> Parse a a
spot p (x:xs) 
  | p x 	= [(x,xs)]
  | otherwise 	= []
spot p []    = []
--  
-- Examples.							
--  
bracket = token '('
dig     =  spot isDigit

-- Succeeds with value given when the input is empty.

endOfInput :: b -> Parse a b
endOfInput x [] = [(x,[])]
endOfInput x _  = []
--  
-- Combining parsers						
--  
--  
-- alt p1 p2 recognises anything recogniseed by p1 or by p2.	
--  
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp
exam1 = (bracket `alt` dig) "234" 
--  
-- Apply one parser then the second to the result(s) of the first.	
--  

(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
-- 	
(>*>) p1 p2 inp 
  = [((y,z),rem2) | (y,rem1) <- p1 inp , (z,rem2)  <- p2 rem1 ]
--  
-- Transform the results of the parses according to the function.	
--  
build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [ (f x,rem) | (x,rem) <- p inp ]
--  
-- Recognise a list of objects.					
--  
-- 	
list :: Parse a b -> Parse a [b]
list p = (succeed []) 
         `alt`
         ((p >*> list p) `build` convert)
         where
         convert = uncurry (:)
--  
-- Some variants...

-- A non-empty list of objects.						
--  
neList   :: Parse a b -> Parse a [b]
neList p = (p  `build` (:[]))
           `alt`
           ((p >*> list p) `build` (uncurry (:)))

-- Zero or one object.

optional :: Parse a b -> Parse a [b]
optional p = (succeed []) 
             `alt`  
             (p  `build` (:[]))

-- A given number of objects.

nTimes :: Int -> Parse a b -> Parse a [b]
nTimes 0 p     = succeed []
nTimes n p     = (p >*> nTimes (n-1) p) `build` (uncurry (:))
--  

-- Monadic parsing on top of this library

newtype SParse a b = SParse { sparse :: (Parse a b) }

instance Monad (SParse a) where
  return x = SParse (succeed x)
  fail s   = SParse none
  (SParse pr) >>= f 
    = SParse (\st -> concat [ sparse (f x) rest | (x,rest) <- pr st ])
