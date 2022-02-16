module Screeps.Game where
import Prelude
import Foreign.Object as F
import Effect (Effect)
import Screeps.Types
import Screeps.Memory as Memory
import Screeps.FFI ( runThisEffFn0, runThisFn1
                   , unsafeField, toMaybe )
import Data.Maybe ( Maybe )
import Data.Either ( Either(..) )
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Encode ( encodeJson )
import Data.Map as Map

foreign import getGameGlobal   ∷ Effect GameGlobal
foreign import createCreepImpl ∷ Spawn → Array BodyPartType → Json
    → (ReturnCode → Either ReturnCode String)
    → (String     → Either ReturnCode String)
    → Effect (Either ReturnCode String)
foreign import rawGetUtility ∷ String → Memory.MemoryGlobal → Int
foreign import rawGetRole ∷ String → Memory.MemoryGlobal → Role

type Cpu = { limit     ∷ Int
           , tickLimit ∷ Int
           , bucket    ∷ Int }

cpu ∷ GameGlobal → Cpu
cpu = unsafeField "cpu"
spawns ∷ GameGlobal → F.Object Spawn
spawns = unsafeField "spawns"

creeps ∷ GameGlobal → F.Object Creep
creeps = unsafeField "creeps"

getUsed ∷ GameGlobal → Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)

getObjectById ∷ ∀ a. GameGlobal → Id a → Maybe a
getObjectById game id
  = toMaybe (runThisFn1 "getObjectById" game id)

getUtility ∷ Memory.MemoryGlobal → String → Int
getUtility mem key = rawGetUtility key mem

getRole ∷ Memory.MemoryGlobal → String → Role
getRole mem key = rawGetRole key mem

rawSpawnCreep ∷ Spawn → Array BodyPartType → String
  → Role → Effect (Either ReturnCode String)
rawSpawnCreep spawn parts name role
  = createCreepImpl spawn parts role' Left Right
  where role' = encodeJson role

rawSpawnCreepAs ∷ Spawn 
  → Array BodyPartType → String → Role
  → Effect (Either ReturnCode String)
rawSpawnCreepAs spawn parts name role
  = createCreepImpl spawn parts role' Left Right
  where role' = encodeJson role

time ∷ GameGlobal → Int
time = unsafeField "time"
