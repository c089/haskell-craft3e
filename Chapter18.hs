-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	Chapter 18
--
-----------------------------------------------------------------------


module Chapter18 where

import Prelude hiding (lookup)
import System.IO 
import Control.Monad.Identity
import Chapter8 (getInt)
import Data.Time
import System.Locale
import System.IO.Unsafe (unsafePerformIO)

-- Programming with monads
-- ^^^^^^^^^^^^^^^^^^^^^^^


-- The basics of input/output
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Reading input is done by getLine and getChar: see Prelude for details.

-- 	getLine :: IO String
-- 	getChar :: IO Char

-- Text strings are written using 
-- 	
-- 	putStr :: String -> IO ()
-- 	putStrLn :: String -> IO ()

-- A hello, world program

helloWorld :: IO ()
helloWorld = putStr "Hello, World!"

-- Simple examples

readWrite :: IO ()

readWrite =
    do
      getLine
      putStrLn "one line read"

readEcho :: IO ()

readEcho =
    do
      line <-getLine
      putStrLn ("line read: " ++ line)


-- Adding a sequence of integers from the input

sumInts :: Integer -> IO Integer

sumInts s
  = do n <- getInt
       if n==0 
          then return s
          else sumInts (s+n)

-- Adding a list of integers, using an accumulator

sumAcc :: Integer -> [Integer] -> Integer

sumAcc s [] = s
sumAcc s (n:ns) 
  = if n==0
       then s
       else sumAcc (s+n) ns


-- Addiing a sequence of integers, courteously.

sumInteract :: IO ()
sumInteract
  = do putStrLn "Enter integers one per line"
       putStrLn "These will be summed until zero is entered"
       sum <- sumInts 0
       putStr "The sum is "
       print sum


-- Further I/O
-- ^^^^^^^^^^^

-- Interaction at the terminal

copyInteract :: IO ()

copyInteract = 
    do
      hSetBuffering stdin LineBuffering
      copyEOF
      hSetBuffering stdin NoBuffering

copyEOF :: IO ()

copyEOF = 
    do 
      eof <- isEOF
      if eof  
        then return () 
        else do line <- getLine 
                putStrLn line
                copyEOF

-- Input and output as lazy lists

-- Reverse all the lines in the input.

listIOprog :: String -> String

listIOprog = unlines . map reverse . lines


-- Generating random numbers

randomInt :: Integer -> IO Integer
randomInt n = 
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)
      
randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt 
      


-- The calculator
-- ^^^^^^^^^^^^^^

-- This is available separately in the Calculator directory.


-- The do notation revisited
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

addOneInt :: IO ()

addOneInt 
  = do line <- getLine
       putStrLn (show (1 + read line :: Int))       

addOneInt' 
  = getLine >>= \line ->
    putStrLn (show (1 + read line :: Int))     

-- Monads for Functional Programming
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The definition of the Monad class
-- 	class Monad m where
-- 	  (>>=)  :: m a -> (a -> m b) -> m b
-- 	  return :: a -> m a
-- 	  fail   :: String -> m a

-- Kelisli composition for monadic functions.

-- (>@>) :: Monad m => (a -> m b) ->
--                     (b -> m c) ->
--                     (a -> m c)

-- f >@> g = \ x -> (f x) >>= g


-- Some examples of monads
-- ^^^^^^^^^^^^^^^^^^^^^^^

-- Some examples from the standard prelude.

-- The list monad

-- 	instance Monad [] where
-- 	  xs >>= f  = concat (map f xs)
-- 	  return x  = [x]
-- 	  zero      = []

-- The Maybe monad

-- 	instance Monad Maybe where
-- 	  (Just x) >>= k  =  k x
-- 	  Nothing  >>= k  =  Nothing
-- 	  return          =  Just


-- The parsing monad

-- 	data SParse a b = SParse (Parse a b)

-- 	instance Monad (SParse a) where
-- 	  return x = SParse (succeed x)
-- 	  zero     = SParse fail
-- 	  (SParse pr) >>= f 
-- 	    = SParse (\s -> concat [ sparse (f x) rest | (x,rest) <- pr st ])

-- 	sparse :: SParse a b -> Parse a b
-- 	sparse (SParse pr) = pr

-- A state monad (the state need not be a table; this example is designed
-- to support the example discussed below.)

type Table a = [a]

data State a b = State (Table a -> (Table a , b))

instance Monad (State a) where

  return x = State (\tab -> (tab,x))

  (State st) >>= f 
    = State (\tab -> let 
                     (newTab,y)    = st tab
                     (State trans) = f y 
                     in
                     trans newTab)


-- Example: Monadic computation over trees
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A type of binary trees.

data Tree a = Nil | Node a (Tree a) (Tree a)
              deriving (Eq,Ord,Show)

-- Summing a tree of integers

-- A direct solution:

sTree :: Tree Integer -> Integer

sTree Nil            = 0
sTree (Node n t1 t2) = n + sTree t1 + sTree t2

-- A monadic solution: first giving a value of type Identity Int ...

sumTree :: Tree Integer -> Identity Integer

sumTree Nil = return 0

sumTree (Node n t1 t2)
  = do num <- return n
       s1  <- sumTree t1
       s2  <- sumTree t2
       return (num + s1 + s2)

-- ... then adapted to give an Int solution

sTree' :: Tree Integer -> Integer

sTree' = identity . sumTree

identity :: Identity a -> a

identity (Identity x) = x

-- Using a state monad in a tree calculation
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The top level function ...

numTree :: Eq a => Tree a -> Tree Integer

-- ... and the function which does all the work:

numberTree :: Eq a => Tree a -> State a (Tree Integer)

-- Its structure mirrors exactly the structure of the earlier program to
-- sum the tree.

numberTree Nil = return Nil

numberTree (Node x t1 t2)
  = do num <- numberNode x
       nt1 <- numberTree t1
       nt2 <- numberTree t2
       return (Node num nt1 nt2)

-- The work of the algorithm is done node by node, hence the function

numberNode :: Eq a => a -> State a Integer

numberNode x = State (nNode x)

--  
-- Looking up a value in the table; will side-effect the table if the value
-- is not present.

nNode :: Eq a => a -> (Table a -> (Table a , Integer))
nNode x table
  | elem x table        = (table      , lookup x table)
  | otherwise           = (table++[x] , integerLength table)
    where
      integerLength = toInteger.length
  
-- Looking up a value in the table when known to be present

lookup :: Eq a => a -> Table a -> Integer

lookup x tab = 
    locate 0 tab
           where
             locate n (y:ys) = 
                 if x==y then n else locate (n+1) ys

-- Extracting a value froma state monad.

runST :: State a b -> b
runST (State st) = snd (st [])

-- The top-level function defined eventually.

numTree = runST . numberTree

-- Example tree

egTree :: Tree String
 
egTree = Node "Moon"
               (Node "Ahmet" Nil Nil)
               (Node "Dweezil"  
                        (Node "Ahmet" Nil Nil) 
                        (Node "Moon" Nil Nil))

