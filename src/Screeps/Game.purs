module Screeps.Game where
import Prelude
import Effect (Effect)
import Screeps.Types (GameGlobal)
import Screeps.FFI (runThisEffFn0, unsafeField)
foreign import getGameGlobal :: Effect GameGlobal

type Cpu = { limit :: Int
           , tickLimit :: Int
           , bucket :: Int }

cpu :: GameGlobal -> Cpu
cpu = unsafeField "cpu"

getUsed :: GameGlobal -> Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)
