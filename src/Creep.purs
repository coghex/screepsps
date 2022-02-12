module Creep where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int ( toStringAs, decimal )
import Data.Map as Map
import Data.List (head)
import Foreign.Object as F
import Screeps.Types
import Screeps.Creep as Creep

processCreeps :: F.Object Creep -> Effect Unit
processCreeps map = log $ show $ Creep.name <$> map
