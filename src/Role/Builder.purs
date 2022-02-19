module Role.Builder where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Screeps.Const ( err_not_in_range, find_sources
                     , find_structures, resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Memory as Memory
import Screeps.Types
import Screeps.Creep ( harvestSource, moveTo
                     , transferToStructure, creepStore
                     , getCreepMem, name )
import Screeps.Room ( room, find, find', storage )
import Screeps.Storage ( storeFreeCapacity, store )
import Screeps.Structure ( structureType )
import Screeps.Spawn ( spawnStoreCapacity )
import Foreign.Object as F

-- | a builder moves between mining for energy and building construction sites
preformBuilder ∷ Creep → Effect Unit
preformBuilder creep = do
  mem ← getCreepMem (name creep)
  let mem' = mem ∷ Either JsonDecodeError (F.Object Json)
  case mem of
    Left err → pure unit
    Right d0 → pure unit
