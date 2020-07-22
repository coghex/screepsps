module Screeps.Memory where
import Prelude
import Data.Either (Either)
import Effect (Effect)
import Screeps.FFI (unsafeGetFieldEff, unsafeSetFieldEff)

foreign import data MemoryGlobal :: Type
foreign import getMemoryGlobal :: Effect MemoryGlobal
foreign import data RawMemoryGlobal :: Type
foreign import getRawMemoryGlobal :: Effect RawMemoryGlobal

get :: forall a. MemoryGlobal -> String -> Effect (Either String a)
get memoryGlobal key = unsafeGetFieldEff key memoryGlobal

set :: forall a. MemoryGlobal -> String -> a -> Effect Unit
set memoryGlobal key val = unsafeSetFieldEff key memoryGlobal val
