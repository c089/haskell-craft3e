-------------------------------------------------------------------------
-- 
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	The basis of the simulation package.
--
-------------------------------------------------------------------------

module Base where


-- The type of input messages. 

data Inmess = No | Yes Arrival Service
	      deriving (Eq,Show)

type Arrival = Int
type Service = Int

-- The type of output messages. 

data Outmess = None | Discharge Arrival Wait Service
	       deriving (Eq,Show)

type Wait = Int
