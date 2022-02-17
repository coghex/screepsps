module Screeps.Room where
import Prelude
import Effect (Effect)
import Data.Maybe ( Maybe )
import Screeps.Structure ( structureType )
import Screeps.Game as Game
import Screeps.Types
import Screeps.FFI ( runThisFn0, runThisFn1
                   , runThisFn2, runThisFn3
                   , unsafeField, toMaybe )
import Foreign.Object as F

-- | returns a list of sources given a room
--findSources ∷ ∀ a. Room → Array (RoomObject a)
--findSources room = find' room find_structures (\s → isSource s)
foreign import setRoomMem :: Room → Int → Int → Effect Unit

find ∷ ∀ a. Room → FindType a → Array a
find = runThisFn1 "find"
-- | returns find when we want to use optional filter
find' ∷ ∀ a. Room → FindType a → FilterFn a → Array a
find' room findType filter = runThisFn2 "find" room findType { filter }

room ∷ ∀ a. RoomObject a → Room
room = unsafeField "room"

storage ∷ Room → Storage
storage = unsafeField "storage"

getTerrain ∷ Room → Terrain
getTerrain = runThisFn0 "getTerrain"

getTerrainAt ∷ ∀ a. TargetPosition a → String → Terrain
getTerrainAt (TargetPt x y)  roomName = runThisFn3 "getTerrainAt" Game.map x y roomName
getTerrainAt (TargetPos pos) roomName = runThisFn2 "getTerrainAt" Game.map pos roomName
getTerrainAt (TargetObj obj) roomName = runThisFn2 "getTerrainAt" Game.map obj roomName
