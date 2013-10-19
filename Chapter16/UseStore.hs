-------------------------------------------------------------------------
--  
-- 	   UseStore.hs
--  
--         Using the abstract data type Store of stores of integers.		
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------


module UseStore where

import Store

-- Testing the exported definitions of the show and equality.					

exam1 = show initial

exam2 = (initial == initial) 

-- Can you check a Store against its representation? You need to uncomment
-- the definition before you use it.

-- checkAbs = (initial == Store [])

-- A complex store.

store3 = update (update (update initial 'a' 4) 'b' 5) 'a' 3

-- Show the store3.

exam3  = show store3 

-- Lookup 'a' in store3; can see that 'a' has the value 3 rather than 4.

exam4  = value store3 'a'
