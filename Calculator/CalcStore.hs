-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
--      CalcStore.hs
--
--      An abstract data type of stores of integers, implemented as
--      a list of pairs of variables and values.			
--
-----------------------------------------------------------------------



module CalcStore 
   ( Store, 
     initial,     -- Store
     value,       -- Store -> Var -> Integer
     update       -- Store -> Var -> Integer -> Store
    ) where

import CalcTypes					

-- The implementation is given by a newtype declaration, with one
-- constructor, taking an argument of type [ (Int,Var) ].

data Store = Sto [ (Integer,Var) ] 

instance Eq Store where 
  (Sto sto1) == (Sto sto2) = (sto1 == sto2)					

instance Show Store where
  showsPrec n (Sto sto) = showsPrec n sto					
--  
initial :: Store 

initial = Sto []

value  :: Store -> Var -> Integer

value (Sto []) v         = 0
value (Sto ((n,w):sto)) v 
  | v==w            = n
  | otherwise       = value (Sto sto) v

update  :: Store -> Var -> Integer -> Store

update (Sto sto) v n = Sto ((n,v):sto)

