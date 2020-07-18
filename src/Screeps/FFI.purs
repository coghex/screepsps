module Screeps.FFI where
import Prelude
import Effect (Effect)

foreign import unsafeField :: forall obj val. String -> obj -> val
foreign import runThisEffFn0 :: forall this a. String -> this -> Effect a
