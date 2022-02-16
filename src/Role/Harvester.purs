module Role.Harvester where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index)
import Data.Maybe (Maybe(..))
import Screeps.Const ( err_not_in_range, find_sources
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Memory as Memory
import Screeps.Types
import Screeps.Creep ( harvestSource, moveTo )
import Screeps.Room ( room, find' )

-- | a harvester moves between energy source and extension, spawn, or tower
preformHarvester ∷ Creep → Effect Unit
preformHarvester creep = do
  freeCapacity ← getFreeCapacity
  if freeCapacity > 0 then do
    let sources = find' (room creep) find_sources isSource --(\s → isSource s)
        isSource structure_spawn     = true
        isSource structure_tower     = true
        isSource structure_extension = true
        isSource _                   = false
    case (findNearest sources) of
      Nothing → pure unit
      Just nearestSource → do
        harv ← harvestSource creep nearestSource
        if harv == err_not_in_range then do
          ret ← moveTo creep (TargetObj nearestSource)
          pure unit
    else pure unit
  else do
    pure unit
--    targets ← findTargets
--    let nearestTarget = findNearestTarget targets
--    if (length targets) > 0 then
--      targ ← creepTransfer nearestTarget
--      if targ == ERR_NOT_IN_RANGE then
--        creepMove nearestTarget
--      else pure unit
--    else pure unit

getFreeCapacity ∷ Effect Int
getFreeCapacity = pure 1
findNearest ∷ ∀ a. Array a → Maybe a
findNearest arr = arr `index` 0
