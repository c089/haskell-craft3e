-------------------------------------------------------------------------
--  
-- 	   StoreFun.hs
--  
--         An abstract data type of stores of integers, implemented as functions.
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------


-- An alternative implementation of Store.hs. Note that although
-- it is equivalent to the list implementation as far as the operations
-- initial, value, update are concerned, it is not possible to compare for
-- equality or to show as a String.

module StoreFun 
   ( Store, 
     initial,     -- Store
     value,       -- Store -> Var -> Integer
     update       -- Store -> Var -> Integer -> Store
    ) where

-- Var is the type of variables.					

type Var = Char

newtype Store = Store (Var -> Integer) 					
--  
initial :: Store 

initial = Store (\v -> 0)

value :: Store -> Var -> Integer

value (Store sto) v = sto v

update  :: Store -> Var -> Integer -> Store

update (Store sto) v n 
  = Store (\w -> if v==w then n else sto w)

