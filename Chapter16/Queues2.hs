-------------------------------------------------------------------------
--  
--         Queues2.hs
--  
--         An abstract data type of queues, implemnted as a list, with
--         new elements added at the beginning of the list.
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------                      

module Queues2 
  ( Queue , 
    emptyQ ,       --  Queue a
    isEmptyQ ,     --  Queue a -> Bool 
    addQ ,         --  a -> Queue a -> Queue a
    remQ           --  Queue a -> (  a , Queue a )
   ) where 

newtype Queue a = Queue [a]
--  
emptyQ :: Queue a

emptyQ = Queue []

isEmptyQ :: Queue a -> Bool

isEmptyQ (Queue []) = True
isEmptyQ _       = False

addQ   :: a -> Queue a -> Queue a

addQ x (Queue xs) = Queue (x:xs)

remQ   :: Queue a -> (  a , Queue a )

remQ q@(Queue xs)
  | not (isEmptyQ q)   = (last xs , Queue (init xs))
  | otherwise          = error "remQ"

