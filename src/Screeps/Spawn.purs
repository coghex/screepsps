module Screeps.Spawn where

import Prelude
import Effect (Effect)
import Screeps.Types
import Screeps.FFI ( unsafeField, toMaybe )
import Data.Maybe (Maybe)

foreign import spawnStoreCapacity ∷ ∀ obj. obj → Int

spawnStore ∷ ∀ a. Structure a → Store
spawnStore = unsafeField "store"
