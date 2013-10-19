-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	Pic.hs
-- 
--      A deep embedding of pictures
--
-----------------------------------------------------------------------

module Pic where

import Pictures

-- Data type representing pictures

data Pic = Horse |
           Above Pic Pic |
           Beside Pic Pic |
           FlipH Pic |
           FlipV Pic 

-- Interpreting a Pic as a Picture

interpretPic :: Pic -> Picture

interpretPic Horse = horse
interpretPic (Above pic1 pic2)
  = above (interpretPic pic1)  (interpretPic pic2)
interpretPic (Beside pic1 pic2)
  = beside (interpretPic pic1)  (interpretPic pic2)
interpretPic (FlipH pic)
  = flipH (interpretPic pic)
interpretPic (FlipV pic)
  = flipV (interpretPic pic)

-- Tidying up a picture ...

-- remove pairs of flips
-- push flips through placement above / beside

tidyPic :: Pic -> Pic

tidyPic (FlipV (FlipV pic)) 
  = tidyPic pic
tidyPic (FlipV (FlipH pic)) 
  = FlipH (tidyPic (FlipV pic)) 

tidyPic (FlipV (Above pic1 pic2))
  = Above (tidyPic (FlipV pic1)) (tidyPic (FlipV pic2)) 
tidyPic (FlipV (Beside pic1 pic2))
  = Beside (tidyPic (FlipV pic2)) (tidyPic (FlipV pic1)) 

tidyPic (FlipH (FlipH pic)) 
  = tidyPic pic
  
tidyPic (FlipH (Above pic1 pic2))
  = Above (tidyPic (FlipH pic2)) (tidyPic (FlipH pic1)) 
tidyPic (FlipH (Beside pic1 pic2))
  = Beside (tidyPic (FlipH pic1)) (tidyPic (FlipH pic2)) 
  
