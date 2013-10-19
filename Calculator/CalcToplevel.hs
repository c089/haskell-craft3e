-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	CalcToplevel.hs
--
-- 	Top-level interaction loop for a calculator
--
-----------------------------------------------------------------------

module CalcToplevel where

import System.IO 

import CalcTypes
import CalcStore
import CalcParseLib
import CalcParse
import CalcEval


calcStep :: Store -> IO Store

calcStep st
  = do line <- getLine
       let comm = calcLine line
       let (val , newSt) = command comm st
       print val
       return newSt


calcSteps :: Store -> IO ()

calcSteps st =
    do
      eof <- isEOF
      if eof
         then return ()
         else do newSt <- calcStep st
                 calcSteps newSt


mainCalc :: IO ()
mainCalc = 
    do
      hSetBuffering stdin LineBuffering
      calcSteps initial
      hSetBuffering stdin NoBuffering



