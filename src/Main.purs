module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Prim.Boolean (True, False)
import Data.Maybe(Maybe(Just,Nothing))
import Data.Either(Either(..))
import Data.Map as Map
import Data.Argonaut.Decode ( printJsonDecodeError, decodeJson )
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Types
import Creep
import Foreign.Object as F

main :: Effect Unit
main = do
  game <- Game.getGameGlobal
  memory <- Memory.getMemoryGlobal
  -- checks every frame if we need to reinitialize everything
  initCheck <- areWeInit memory
  case initCheck of
    Nothing -> do
       initCorpsegrinder memory LoopGo
    Just status -> do
       runCorpsegrindeer game memory status

-- | checks to see if we need to initialize the memory
areWeInit ∷ Memory.MemoryGlobal -> Effect (Maybe LoopStatus)
areWeInit memory = do
  loopStatus <- Memory.get memory "loopStatus"
  case loopStatus of
      Left err -> do
         log $ printJsonDecodeError err
         pure Nothing
      Right a0 -> case a0 of
                    Nothing -> pure $ Just LoopGo
                    Just ls -> pure ls

-- | simple initialization of memory
initCorpsegrinder ∷ Memory.MemoryGlobal -> LoopStatus -> Effect Unit
initCorpsegrinder memory status = do
  log "starting the corpsegrinder..."
  Memory.set memory "loopStatus" $ Just status
  Memory.set memory "utility" 0

-- | runs as the main loop function
runCorpsegrindeer ∷ GameGlobal -> Memory.MemoryGlobal -> LoopStatus -> Effect Unit
runCorpsegrindeer game memory LoopGo = do
  let creeps = Game.creeps game
  processCreeps creeps game memory
  -- TODO: finish tower code
  let tower = Game.getObjectById game (Id "TOWER_ID")
  case tower of
    Just t0 -> log "tower code here"
    Nothing -> pure unit
runCorpsegrindeer game memory LoopStop = do
    -- TODO: reset memory here
    log "stopping loop"
runCorpsegrindeer game memory LoopNULL = do
    log "null loop"
