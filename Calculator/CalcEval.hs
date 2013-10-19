-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	CalcEval.hs
--
-- 	Evaluating expressions and commands
--
-----------------------------------------------------------------------


module CalcEval where

import CalcTypes
import CalcStore

eval :: Expr -> Store -> Integer

eval (Lit n) st = n
eval (Var v) st = value st v
eval (Op op e1 e2) st
  = opValue op v1 v2
    where
    v1 = eval e1 st
    v2 = eval e2 st

opValue :: Ops -> Integer -> Integer -> Integer

opValue Add = (+)
opValue Sub = (-) 
opValue Mul = (*) 
opValue Div = div 
opValue Mod = mod

command :: Command -> Store -> (Integer,Store)

command Null st     = (0 , st)
command (Eval e) st = (eval e st , st)
command (Assign v e) st 
  = (val , newSt)
    where
    val   = eval e st
    newSt = update st v val

