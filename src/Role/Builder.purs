module Role.Builder where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, getField)
import Screeps.Const ( err_not_in_range, find_sources
                     , find_construction_sites, resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Memory as Memory
import Screeps.Types
import Screeps.Creep ( harvestSource, moveTo, build
                     , transferToStructure, creepStore
                     , getAllCreepMem, setCreepMem, name )
import Screeps.Room ( room, find, find', storage )
import Screeps.Storage ( storeFreeCapacity, store, storeGet )
import Screeps.Structure ( structureType )
import Screeps.Spawn ( spawnStoreCapacity )
import Foreign.Object as F

-- | a builder moves between mining for energy and building construction sites
preformBuilder ∷ Creep → Effect Unit
preformBuilder creep = do
  mem ← getAllCreepMem (name creep)
  case mem of
    Left err → do
      log $ "creep error: " <> (show err)
      pure unit
    Right d0 → preformBuilderF creep d0
preformBuilderF ∷ Creep → F.Object Json → Effect Unit
preformBuilderF creep mem = do
  case (getField mem "building") of
    -- this means its a new builder, so set the memory
    -- accordingly first, then continue
    Left  _        → do
      setCreepMem (name creep) "building" false
      preformBuilderFF creep false
    -- if we are already a builder this will be set already
    Right building → preformBuilderFF creep building

preformBuilderFF ∷ Creep → Boolean → Effect Unit
preformBuilderFF creep building = if building then do
    let energy       = storeGet (creepStore creep) resource_energy
        freeCapacity = storeFreeCapacity (creepStore creep)
    if energy <= 0 then do
      setCreepMem (name creep) "building" false
    else if freeCapacity == 0 then do
      setCreepMem (name creep) "building" true
    else pure unit
    let targets = find (room creep) find_construction_sites
    case (findNearest targets) of
      Nothing → pure unit
      Just nearestTarget → if (length targets > 0) then do
        res ← build creep nearestTarget
        if (res == err_not_in_range) then do
          ret ← moveTo creep (TargetObj nearestTarget)
          pure unit
        else pure unit
   else pure unit
  else do
    let sources = find (room creep) find_sources
    case (findNearest sources) of
      Nothing → pure unit
      Just nearestSource → do
        ret ← harvestSource creep nearestSource
        if (ret == err_not_in_range) then do
          ret ← moveTo creep (TargetObj nearestSource)
          pure unit
        else pure unit

-- TODO: write this function
findNearest ∷ ∀ a. Array a → Maybe a
findNearest arr = arr `index` 0
