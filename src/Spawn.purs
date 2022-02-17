module Spawn where
import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Screeps.Types
import Screeps.Memory as Memory
import Screeps.Game as Game
import Screeps.Room ( room, setRoomMem )
import Foreign.Object as F

initSpawn1 ∷ F.Object Creep → GameGlobal → Memory.MemoryGlobal → Effect Unit
initSpawn1 hash game mem = do
  let spawnslist = Game.spawns game
      spawn1     = F.lookup "Spawn1" spawnslist
  case spawn1 of
    Nothing → pure unit
    Just s1 → do
      let r = room s1
      setRoomMem r 0 0
