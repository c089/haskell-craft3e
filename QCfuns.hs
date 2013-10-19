-------------------------------------------------------------------------
--
--	Haskell: The Craft of Functional Programming
--	Simon Thompson
--	(c) Addison-Wesley, 1996-2011.
--
--	QCfuns
--
-------------------------------------------------------------------------

module QCfuns where

import Test.QuickCheck
import System.IO.Unsafe -- for unsafePerformIO

-- Sampling and showing functions

sampleFun :: (Arbitrary a,Show a, Show b)  => (a -> b) -> IO String

sampleFun f =
    do
      inputs <- sample' arbitrary
      let list = [ (a,f a) | a <- inputs ]
      return $ showMap list

showMap :: (Show a, Show b) => [(a,b)] -> String

showMap [] = "\n"
showMap [(a,b)] = showPair (a,b) ++ "\n"
showMap (p:ps)  = showPair p ++ " ," ++ showMap ps

showPair :: (Show a, Show b) => (a,b) -> String

showPair (a,b) = "("++show a ++ "|->" ++ show b ++ ")"

instance (Arbitrary a, Show a, Show b) => Show (a -> b) where
    show = unsafePerformIO . sampleFun