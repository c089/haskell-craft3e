-------------------------------------------------------------------------
-- 
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
--      Case study: Parsing expressions	
-- 
--      Note that this is not a monadic approach to parsing.	
-- 
---------------------------------------------------------------------------                                                     

module ParsingBasics where

import Data.Char

infixr 5 >*>
--  
-- Syntactic types							
--  
type Var = Char
data Expr = Lit Int | Var Var | Op Op Expr Expr
data Op   = Add | Sub | Mul | Div | Mod
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
list p = (succeed []) `alt`
         ((p >*> list p) `build` convert)
         where
         convert = uncurry (:)
--  
-- From the exercises...						
--  
neList   :: Parse a b -> Parse a [b]
neList = neList		 	 -- dummy definition
optional :: Parse a b -> Parse a [b]
optional = optional	 	 -- dummy definition
nTimes :: Int -> Parse a b -> Parse a [b]
nTimes = nTimes		 	 -- dummy definition
--  
-- A parser for expressions					
--  
--  
-- The parser has three components, corresponding to the three	
-- clauses in the definition of the syntactic type.		
--  
parser :: Parse Char Expr
parser = (litParse `alt` varParse) `alt` opExpParse
--  
-- Spotting variables.						
--  
varParse :: Parse Char Expr
varParse = spot isVar `build` Var

isVar :: Char -> Bool
isVar x = ('a' <= x && x <= 'z')
--  
-- Parsing (fully bracketed) operator applications.		
--  
opExpParse 
  = (token '(' >*>
     parser    >*>
     spot isOp >*>
     parser    >*>
     token ')') 
     `build` makeExpr

makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

isOp :: Char -> Bool
isOp = isOp		  	 -- dummy definition

charToOp :: Char -> Op
charToOp = charToOp	  	 -- dummy definition

--  
-- A number is a list of digits with an optional ~ at the front. 
--  
litParse 
  = ((optional (token '~')) >*>
     (neList (spot isDigit)))
     `build` (charlistToExpr.join) 
     where
     join = uncurry (++)
--  
-- From the exercises...						
--  
charlistToExpr :: [Char] -> Expr
charlistToExpr = charlistToExpr 	 -- dummy definition
--  
-- A grammar for unbracketed expressions.				
-- 								
-- eXpr  ::= Int | Var | (eXpr Op eXpr) |				
--           lexpr mop mexpr | mexpr aop eXpr			
-- lexpr ::= Int | Var | (eXpr Op eXpr)				
-- mexpr ::= Int | Var | (eXpr Op eXpr) |	lexpr mop mexpr		
-- mop   ::= 'a' | '/' | '\%'					
-- aop   ::= '+' | '-'						
--  
--  
-- The top-level parser						
--  
topLevel :: Parse a b -> [a] -> b
topLevel p inp
  = case results of
      [] -> error "parse unsuccessful"
      _  -> head results
    where
    results = [ found | (found,[]) <- p inp ]
--  
-- The type of commands.						
--  
data Command = Eval Expr | Assign Var Expr | Null
commandParse :: Parse Char Command
commandParse = commandParse 	 -- dummy definition
--  
-- From the exercises.						
--  
-- tokenList :: [a] -> Parse a [a]
-- spotWhile :: (a -> Bool) -> Parse a [a]
