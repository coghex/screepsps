module Role.Harvester where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Screeps.Const ( err_not_in_range, find_sources
                     , find_structures, resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Memory as Memory
import Screeps.Types
import Screeps.Creep ( harvestSource, moveTo, transferToStructure, creepStore )
import Screeps.Room ( room, find, find', storage )
import Screeps.Storage ( storeFreeCapacity, store )
import Screeps.Structure ( structureType )
import Screeps.Spawn ( spawnStoreCapacity )

-- | a harvester moves between energy source and extension, spawn, or tower
preformHarvester ∷ Creep → Effect Unit
preformHarvester creep = do
  let freeCapacity = storeFreeCapacity (creepStore creep)
  if freeCapacity > 0 then do
    let sources = find (room creep) find_sources
    case (findNearest sources) of
      Nothing → pure unit
      Just nearestSource → do
        harv ← harvestSource creep nearestSource
        if harv == err_not_in_range then do
          ret ← moveTo creep (TargetObj nearestSource)
          pure unit
    else pure unit
  else do
    let targets = find' (room creep) find_structures hasFreeSpace
--    log $ "num targets: " <> (show (length targets))
    case (findNearest targets) of
      Nothing → pure unit
      Just nearestTarget → if (length targets) > 0 then do
          targ ← transferToStructure creep nearestTarget resource_energy
          if targ == err_not_in_range then do
            ret ← moveTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit

hasFreeSpace ∷ ∀ a. Structure a → Boolean
hasFreeSpace structure = ((isStorable structure (structureType structure))) && ((spawnStoreCapacity structure) > 0)
isStorable ∷ ∀ a. Structure a → StructureType → Boolean
isStorable structure structure_spawn     = true
isStorable structure structure_tower     = true
isStorable structure structure_extension = true
isStorable _         _                   = false

-- TODO: write this function
findNearest ∷ ∀ a. Array a → Maybe a
findNearest arr = arr `index` 0
