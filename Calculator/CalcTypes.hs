-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	CalcTypes.hs
--
-- 	Types for the calculator
--
-----------------------------------------------------------------------


module CalcTypes where

data Expr = Lit Integer | Var Var | Op Ops Expr Expr	deriving (Eq,Show)

data Ops  = Add | Sub | Mul | Div | Mod	 		deriving (Eq,Show)

type Var  = Char				

data Command = Eval Expr | Assign Var Expr | Null	deriving (Eq,Show)



