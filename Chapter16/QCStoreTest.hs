-------------------------------------------------------------------------
--  
--         QCStoreTest.hs	
--  
--         QuickCheck tests for stores.							-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------


module QCStoreTest  where

import StoreTest
import Test.QuickCheck

prop_Update1 :: Char -> Integer -> Store -> Bool

prop_Update1 ch int st =
    value (update st ch int) ch == int

prop_Update2 :: Char -> Char -> Integer -> Store -> Bool

prop_Update2 ch1 ch2 int st =
    ch1 == ch2 || value (update st ch2 int) ch1 == value st ch1

prop_Initial :: Char -> Bool

prop_Initial ch =
   value initial ch == 0
