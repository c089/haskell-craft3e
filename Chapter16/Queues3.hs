-------------------------------------------------------------------------
--  
--         Queues3.hs
--  
--         An abstract data type of queues, implemnted as two lists, with
--         new elements added at the beginning of the second list.		
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------             

module Queues3 
  ( Queue , 
    emptyQ ,       --  Queue a
    isEmptyQ ,     --  Queue a -> Bool 
    addQ ,         --  a -> Queue a -> Queue a
    remQ           --  Queue a -> (  a , Queue a )
   ) where 

data Queue a = Queue [a] [a]

emptyQ :: Queue a

emptyQ = Queue [] []

isEmptyQ :: Queue a -> Bool

isEmptyQ (Queue [] []) = True
isEmptyQ _          = False

addQ   :: a -> Queue a -> Queue a

addQ x (Queue xs ys) = Queue xs (x:ys)

remQ   :: Queue a -> (  a , Queue a )

remQ (Queue (x:xs) ys)    = (x , Queue xs ys)
remQ (Queue [] ys@(z:zs)) = remQ (Queue (reverse ys) [])
remQ (Queue [] [])        = error "remQ"

