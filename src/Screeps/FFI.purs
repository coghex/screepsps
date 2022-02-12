module Screeps.FFI where
import Prelude
import Effect (Effect)

foreign import unsafeField :: forall obj val. String -> obj -> val
foreign import unsafeGetFieldEff :: forall obj val. String -> obj -> Effect val
foreign import unsafeSetFieldEff :: forall obj val. String -> obj -> val -> Effect Unit
foreign import unsafeDeleteFieldEff :: forall obj. String -> obj -> Effect Unit
foreign import runThisEffFn0 :: forall this a. String -> this -> Effect a
foreign import runThisEffFn1 :: forall this a b. String -> this -> a -> Effect b
foreign import runThisFn0 :: forall this a. String -> this -> a
foreign import runThisFn1 :: forall this a b. String -> this -> a -> b
