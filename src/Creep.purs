module Creep where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int ( toStringAs, decimal )
import Data.Map as Map
import Data.List (head)
import Screeps.Types
import Screeps.Creep as Creep

processCreeps :: Map.Map String Creep -> Effect Unit
processCreeps map = log "blop"
