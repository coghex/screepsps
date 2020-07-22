module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Screeps.Game as Game
import Screeps.Memory as Memory
import Foreign.Object as F

main :: Effect Unit
main = do
  game <- Game.getGameGlobal
  memory <- Memory.getMemoryGlobal
  Memory.set memory "init" "true"
  init <- Memory.get memory "init"
  let spawn = F.lookup "Spawn1" (Game.spawns game)
  log init
