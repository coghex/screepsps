module Screeps.Room where
import Prelude
import Effect (Effect)
import Screeps.Structure ( structureType )
import Screeps.Types
import Screeps.FFI ( runThisFn2, unsafeField )
import Foreign.Object as F

-- | returns a list of sources given a room
--findSources ∷ ∀ a. Room → Array (RoomObject a)
--findSources room = find' room find_structures (\s → isSource s)

-- | returns find when we want to use optional filter
find' ∷ ∀ a. Room → FindType a → FilterFn a → Array a
find' room findType filter = runThisFn2 "find" room findType { filter }

room ∷ ∀ a. RoomObject a → Room
room = unsafeField "room"
