-------------------------------------------------------------------------
-- 
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	The server ADT: its signature is given in comments in the module
-- 	header.
--
-------------------------------------------------------------------------

module ServerState 

  ( ServerState ,
    addToQueue,     -- Int -> Inmess -> ServerState -> ServerState
    serverStep,     -- ServerState -> ( ServerState , [Outmess] )
    simulationStep, -- ServerState -> Inmess -> ( ServerState , [Outmess] ) 
    serverStart,    -- ServerState
    serverSize,     -- ServerState -> Int
    shortestQueue   -- ServerState -> Int
  ) where

import Base		-- for the base types of the system
import QueueState	-- for the queue type

-- The server consists of a collection of queues, accessed by integers from 0.

newtype ServerState = SS [QueueState] 
                         deriving (Eq, Show)

-- Adding an element to one of the queues. It uses the function addMessage from the 
-- QueueState abstract type.

addToQueue :: Int -> Inmess -> ServerState -> ServerState
--  
addToQueue n im (SS st)
  = SS (take n st ++ [newQueueState] ++ drop (n+1) st)
    where
    newQueueState = addMessage im (st!!n)

-- A step of the server is given by making a step in each of the constituent
-- queues, and concatenating together the output messages they produce.

serverStep :: ServerState -> ( ServerState , [Outmess] )

serverStep (SS [])
  = (SS [],[])
serverStep (SS (q:qs)) 
  =  (SS (q':qs') , mess++messes)
    where
    (q' , mess)       = queueStep  q
    (SS qs' , messes) = serverStep (SS qs)

-- In making a simulation step, we perform a server step, and then add the
-- incoming message, if it indicates an arrival, to the shortest queue. 

simulationStep  
  :: ServerState -> Inmess -> ( ServerState , [Outmess] )

simulationStep servSt im 
  = (addNewObject im servSt1 , outmess)
    where
    (servSt1 , outmess) = serverStep servSt

-- Adding the message to the shortest queue is done by addNewObject, which
-- is not in the signature. The reason for this is that it can be defined using
-- the operations addToQueue and shortestQueue.

addNewObject :: Inmess -> ServerState -> ServerState

addNewObject No servSt = servSt

addNewObject (Yes arr wait) servSt
  = addToQueue (shortestQueue servSt) (Yes arr wait) servSt

-- The start state of the server.

serverStart :: ServerState
serverStart = SS (replicate numQueues queueStart) 

-- The size of the server.

serverSize :: ServerState -> Int
serverSize (SS xs) = length xs

-- The shortest queue in the server.

shortestQueue :: ServerState -> Int
shortestQueue (SS [q]) = 0
shortestQueue (SS (q:qs)) 
  | (queueLength (qs!!short) <= queueLength q)   = short+1
  | otherwise                                    = 0
      where
      short = shortestQueue (SS qs)

-- The number of queues in the simulation

numQueues :: Int
numQueues = 4
