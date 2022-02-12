module Screeps.Game where
import Foreign.Object as F
import Effect (Effect)
import Screeps.Types
import Screeps.FFI (runThisEffFn0, runThisFn1, unsafeField, toMaybe)
import Data.Maybe ( Maybe )
import Data.Map as Map

foreign import getGameGlobal :: Effect GameGlobal

type Cpu = { limit :: Int
           , tickLimit :: Int
           , bucket :: Int }

cpu :: GameGlobal -> Cpu
cpu = unsafeField "cpu"
spawns :: GameGlobal -> F.Object Spawn
spawns = unsafeField "spawns"

creeps :: GameGlobal -> F.Object Creep
creeps = unsafeField "creeps"

getUsed :: GameGlobal -> Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)

getObjectById :: forall a. GameGlobal -> Id a -> Maybe a
getObjectById game id = toMaybe (runThisFn1 "getObjectById" game id)
