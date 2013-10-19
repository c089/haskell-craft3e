------------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 2011.
-- 
-- 	Chapter 2
-- 
-- 	The example script FirstScript.hs is provided separately,
--      as are the Pictures.hs and PicturesSVG.hs modules.
--
------------------------------------------------------------------------------

module Chapter2 where
import Chapter1

-- Some example expressions

ex1, ex2 :: Integer
ex1 = double 32 - square (size - double 3)
ex2 = double 320 - square (size - double 6)

-- Some examples of expressions which cause errors; that's why
-- they appear as comments and not as Haskell text.
-- 
-- 	2+(3+4
-- 	2+(3+4))
-- 	double square
-- 	4 double
-- 	4 5
-- 	4 `div` (3*2-6)
