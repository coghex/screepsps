module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Prim.Boolean (True, False)
import Data.Maybe(Maybe(Just,Nothing))
import Data.Map as Map
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Types
import Creep
import Foreign.Object as F

main :: Effect Unit
main = do
  game <- Game.getGameGlobal
  memory <- Memory.getMemoryGlobal
  Memory.set memory "utility" 0
--  init <- Memory.get memory "utility"
  let spawn = F.lookup "Spawn1" (Game.spawns game)
  let creeps = Game.creeps game
  processCreeps creeps game memory
  -- TODO: finish tower code
  let tower = Game.getObjectById game (Id "TOWER_ID")
  case tower of
    Just t0 -> log "tower code here"
    Nothing -> log "no tower code needed"
