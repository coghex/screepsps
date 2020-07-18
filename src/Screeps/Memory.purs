module Screeps.Memory where
import Prelude
import Effect (Effect)

foreign import data MemoryGlobal :: Type
foreign import getMemoryGlobal :: forall e. Effect MemoryGlobal
foreign import data RawMemoryGlobal :: Type
foreign import getRawMemoryGlobal :: forall e. Effect RawMemoryGlobal
