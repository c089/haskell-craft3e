---------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 9
--
---------------------------------------------------------------------

-- Reasoning about programs
-- ^^^^^^^^^^^^^^^^^^^^^^^^

module Chapter9 where

import Prelude hiding (sum,length,(++),reverse,unzip)
import Test.QuickCheck


-- Testing and verification
-- ^^^^^^^^^^^^^^^^^^^^^^^^
-- A function supposed to give the maximum of three (integer) values.

mysteryMax :: Integer -> Integer -> Integer -> Integer
mysteryMax x y z
  | x > y && x > z      = x
  | y > x && y > z      = y
  | otherwise           = z

prop_mystery :: Integer -> Integer -> Integer -> Bool

prop_mystery x y z =
    mysteryMax x y z == (x `max` y) `max` z

-- Definedness and termination
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A factorial function, giving an undefined result on negative integers.

fact :: Integer -> Integer
fact n
  | n==0        = 1
  | otherwise   = n * fact (n-1)

-- An infinite list

posInts :: [Integer]
posInts = [1, 2 .. ]


-- Induction
-- ^^^^^^^^^

-- The sum function, defined recursively.

sum :: [Integer] -> Integer

sum []     = 0					-- (sum.1)
sum (x:xs) = x + sum xs				-- (sum.2)

-- Double every element of an integer list.

doubleAll :: [Integer] -> [Integer]

doubleAll []     = []				-- (doubleAll.1)
doubleAll (z:zs) = 2*z : doubleAll zs		-- (doubleAll.2)

-- The property linking the two:
-- 	sum (doubleAll xs) = 2 * sum xs			-- (sum+dblAll)

prop_SumDoubleAll :: [Integer] -> Bool

prop_SumDoubleAll xs =
    sum (doubleAll xs) == 2 * sum xs

-- Other functions used in the examples
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The definitions given here use explicit recursion, rather than applying 
-- higher-order functions as may happen in the Prelude definitions.

length :: [a] -> Int

length []     = 0				-- (length.1)
length (z:zs) = 1 + length zs			-- (length.2)
 
(++) :: [a] -> [a] -> [a]

[]     ++ zs = zs				-- (++.1)
(w:ws) ++ zs = w:(ws++zs)			-- (++.2)

-- QuickCheck property

prop_lengthPlusPlus :: [a] -> [a] -> Bool

prop_lengthPlusPlus xs ys =
    length (xs ++ ys) == length xs + length ys

reverse :: [a] -> [a]

reverse []     = []				-- (reverse.1)
reverse (z:zs) = reverse zs ++ [z]		-- (reverse.2)

-- QuickCheck properties
-- Why does prop_reversePlusPlus' not fail?  Because a defaults to ().
-- See note on "QuickCheck and properties over [a]" at the end of 
-- Section 9.6.

prop_reversePlusPlus' :: Eq a => [a] -> [a] -> Bool

prop_reversePlusPlus' xs ys =
    reverse (xs ++ ys) == reverse xs ++ reverse ys

-- The "right" property here.

prop_reversePlusPlusOops :: [Integer] -> [Integer] -> Bool

prop_reversePlusPlusOops xs ys =
    reverse (xs ++ ys) == reverse xs ++ reverse ys

-- The "right" property here.

prop_reversePlusPlus :: [Integer] -> [Integer] -> Bool

prop_reversePlusPlus xs ys =
    reverse (xs ++ ys) == reverse ys ++ reverse xs

-- Associativity of ++

prop_assocPlusPlus :: [Integer] -> [Integer] -> [Integer] -> Bool

prop_assocPlusPlus xs ys zs =
     (xs ++ ys) ++ zs == xs ++ (ys ++ zs)



unzip :: [(a,b)] -> ([a],[b])

unzip [] = ([],[])
unzip ((x,y):ps) 
  = (x:xs,y:ys)
    where
    (xs,ys) = unzip ps                   


-- Generalizing the proof goal
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The shunting function

shunt :: [a] -> [a] -> [a]

shunt []     ys = ys				-- (shunt.1)
shunt (x:xs) ys = shunt xs (x:ys) 		-- (shunt.2)

-- QuickCheck property of shunt.

prop_shunt :: [Integer] -> [Integer] -> Bool

prop_shunt xs zs =
    shunt (shunt xs zs) [] == shunt zs xs

-- Alternative reverse.

rev :: [a] -> [a]

rev xs = shunt xs []				-- (rev.1)

-- Do they always match?

prop_reverses :: [Integer] -> Bool

prop_reverses xs =
    reverse xs == rev xs

-- An alternative definition of the factorial function.

fac2 :: Integer -> Integer

fac2 n = facAux n 1

facAux :: Integer -> Integer -> Integer

facAux 0 p = p
facAux n p = facAux (n-1) (n*p)

-- QuickChecking the two factorials:

prop_facs' :: Integer -> Bool

prop_facs' x =
    fact x == fac2 x 

prop_facs :: Integer -> Bool

prop_facs x =
    (x<0) || fact x == fac2 x      