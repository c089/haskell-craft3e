-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
--      RegExp.hs
-- 
-- 	Regular Expressions
--
-----------------------------------------------------------------------

module RegExp where

type RegExp = String -> Bool

char :: Char -> RegExp

epsilon = (=="")

char ch = (==[ch])

(|||) :: RegExp -> RegExp ->  RegExp

e1 ||| e2 = 
    \x -> e1 x || e2 x

(<*>) :: RegExp -> RegExp ->  RegExp

e1 <*> e2 =
    \x -> or [ e1 y && e2 z | (y,z) <- splits x ]

(<**>) :: RegExp -> RegExp ->  RegExp

e1 <**> e2 =
    \x -> or [ e1 y && e2 z | (y,z) <- fsplits x ]

splits xs = [splitAt n xs | n<-[0..len]]
    where
      len = length xs

star :: RegExp -> RegExp

star p = epsilon ||| (p <**> star p)
--           epsilon ||| (p <*> star p)
-- is OK as long as p can't have epsilon match

fsplits xs = tail (splits xs)

-- a = char 'a'

-- b = char 'b'

infixr 7 :*:
infixr 5 :|:

data RE = Eps |
          Ch Char |
          RE :|: RE |
          RE :*: RE |
          St RE |
          Plus RE
          deriving(Eq,Show)

evens = St two
two = (a :|: b) :*: (a :|: b)
          
a = Ch 'a'
b = Ch 'b'

-- interp: RE -> RegExp: exercise.

-- Value recursion
--  Eunmerating strings matching a regexp

enumerate :: RE -> [String]

enumerate Eps = [""]
enumerate (Ch ch) = [[ch]]
enumerate (re1 :|: re2)
    = enumerate re1 `interleave` enumerate re2
enumerate  (re1 :*: re2)
    = enumerate re1 `cartesian` enumerate re2
enumerate (St re)
    = result 
      where
        result =
            [""] ++ (enumerate re `cartesian` result)

-- Auxiliary functions
-- interleave and product for potentially infinite lists

interleave :: [a] -> [a] -> [a]

interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs
        
cartesian :: [[a]] -> [[a]] -> [[a]]

cartesian [] ys = []
cartesian (x:xs) ys 
    = [ x++y | y<-ys ] `interleave` cartesian xs ys
    
-- Recursive regular expressions

anbn :: RE

anbn = Eps :|: (a :*: (anbn :*: b))

-- Extending the implementation

plus :: RE -> RE
plus re = re :*: St re

-- Simplification

simplify :: RE -> RE

simplify (St (St re)) = simplify (St re)
simplify (Plus (St re)) = simplify (St re)
simplify (St (Plus re)) = simplify (St re)
simplify (re1 :|: re2) =
    if sre1==sre2 then sre1 else sre1 :|: sre2 
          where
            sre1 = simplify re1; sre2 = simplify re2
simplify re = re

-- smart constructors

starC :: RE -> RE
starC (St re) = re
starC (Plus re) = re
starC re = St re
