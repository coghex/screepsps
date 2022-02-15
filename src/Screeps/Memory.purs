module Screeps.Memory where
import Prelude
import Effect (Effect)
import Screeps.FFI (unsafeGetFieldEff, unsafeSetFieldEff)
import Data.Tuple (Tuple)
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)

foreign import data MemoryGlobal :: Type
foreign import getMemoryGlobal :: Effect MemoryGlobal
foreign import data RawMemoryGlobal :: Type
foreign import getRawMemoryGlobal :: Effect RawMemoryGlobal
foreign import setCreepsUtl :: String -> Int -> Effect Unit
foreign import getCreepsUtl :: Effect (Array Int)

set :: forall a. (EncodeJson a) => MemoryGlobal -> String -> a -> Effect Unit
set memoryGlobal key val = unsafeSetFieldEff key memoryGlobal $ encodeJson val
get :: forall a. (DecodeJson a) => MemoryGlobal -> String -> Effect (Either JsonDecodeError a)
get memoryGlobal key = decodeJson <$> unsafeGetFieldEff key memoryGlobal
