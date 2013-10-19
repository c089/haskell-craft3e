-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	CalcParse.hs
--
-- 	Parsing expressions and commands
--
-----------------------------------------------------------------------

module CalcParse where

import Data.Char

import CalcTypes
import CalcParseLib

-- A parser for expressions					
--  
--  
-- The parser has three components, corresponding to the three	
-- clauses in the definition of the syntactic type.		
--  
parseExpr :: Parse Char Expr
parseExpr = (litParse `alt` varParse) `alt` opExpParse
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
     parseExpr >*>
     spot isOp >*>
     parseExpr >*>
     token ')') 
     `build` makeExpr

makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

isOp :: Char -> Bool
isOp ch = elem ch "+-*/%"

charToOp :: Char -> Ops
charToOp ch 
  = case ch of
      '+' -> Add
      '-' -> Sub
      '*' -> Mul
      '/' -> Div
      '%' -> Mod

--  
-- A number is a list of digits with an optional ~ at the front. 
--  
litParse 
  = ((optional (token '~')) >*>
     (neList (spot isDigit)))
     `build` (charListToExpr.join) 
     where
     join = uncurry (++)

-- Converting strings representing numbers into numbers
--  
charListToExpr :: [Char] -> Expr
charListToExpr = Lit . charListToInt 

charListToInt :: [Char] -> Integer
charListToInt ('~':rest) = - (charListToNat rest)
charListToInt other = charListToNat other

charListToNat :: [Char] -> Integer
charListToNat [] = 0
charListToNat (ch:rest) 
  = charToNat ch * 10^(length rest) + charListToNat rest

charToNat :: Char -> Integer
charToNat ch =
    toInteger $
              if nch < n0 + 10 
                 then nch - n0
                 else n0
              where
                nch = fromEnum ch 
                n0  = fromEnum '0'						

--  
-- The top-level parser						
--  
-- the b value is the result to be returned if there's no successful parse
-- otherwise return the result of the first successful parse

topLevel :: Parse a b -> b -> [a] -> b
topLevel p defaultVal inp
  = case results of
      [] -> defaultVal
      _  -> head results
    where
    results = [ found | (found,[]) <- p inp ]

-- A parse for the type of commands.						
--  

parseCommand :: Parse Char Command
parseCommand 
  = ((parseExpr `build` Eval)
    `alt`
    (((spot isVar) >*> 
     (token ':') >*> 
     parseExpr) `build` makeComm))
     `alt`
     endOfInput Null

makeComm (v,(_,e)) = Assign v e

-- This is the function which gets used in a top-level interaction.....

calcLine :: String -> Command

calcLine = topLevel parseCommand Null
--  

opExpParseM :: SParse Char Expr

opExpParseM =
    do
      tokenM '('
      e1 <- parseExprM 
      bop <- spotM isOp
      e2 <- parseExprM
      tokenM ')'
      return (Op (charToOp bop) e1 e2)

tokenM = SParse . token
spotM  = SParse . spot
parseExprM = SParse parseExpr
