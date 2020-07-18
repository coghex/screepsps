module Screeps.Game where
import Prelude
import Foreign.Object as F
import Effect (Effect)
import Screeps.Types (GameGlobal, Spawn)
import Screeps.FFI (runThisEffFn0, unsafeField)
foreign import getGameGlobal :: Effect GameGlobal

type Cpu = { limit :: Int
           , tickLimit :: Int
           , bucket :: Int }

cpu :: GameGlobal -> Cpu
cpu = unsafeField "cpu"
spawns :: GameGlobal -> F.Object Spawn
spawns = unsafeField "spawns"

getUsed :: GameGlobal -> Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)
