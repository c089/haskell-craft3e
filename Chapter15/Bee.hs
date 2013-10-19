module Bee where 

import Ant hiding ( anteater )
import qualified Ant 

honeyEater = Ant.anteater

beekeeper y = honeyEater y + 1

