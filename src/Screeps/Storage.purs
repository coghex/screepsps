module Screeps.Storage where
import Prelude
import Screeps.Types
import Screeps.FFI (unsafeField,runThisFn0)

storeFreeCapacity ∷ Store → Int
storeFreeCapacity = runThisFn0 "getFreeCapacity"

storeCapacity ∷ Store → Int
storeCapacity = unsafeField "storeCapacity"

store ∷ Storage → Store
store = unsafeField "store"

storeGet ∷ Storage → ResourceType → Int
storeGet s (ResourceType res) = unsafeField res (store s)
