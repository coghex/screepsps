module Screeps.FFI where
import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(Just, Nothing), isJust, fromJust, maybe)
import Data.Function.Uncurried ( Fn3, runFn3 )

foreign import unsafeField :: forall obj val. String -> obj -> val
foreign import unsafeGetFieldEff :: forall obj val. String -> obj -> Effect val
foreign import unsafeSetFieldEff :: forall obj val. String -> obj -> val -> Effect Unit
foreign import unsafeDeleteFieldEff :: forall obj. String -> obj -> Effect Unit
foreign import runThisEffFn0 :: forall this a. String -> this -> Effect a
foreign import runThisEffFn1 :: forall this a b. String -> this -> a -> Effect b
foreign import runThisEffFn2 :: forall this a b c. String -> this -> a -> b -> Effect c
foreign import runThisFn0 :: forall this a. String -> this -> a
foreign import runThisFn1 :: forall this a b. String -> this -> a -> b

foreign import data NullOrUndefined :: Type -> Type
foreign import null :: forall a. NullOrUndefined a
foreign import undefined :: forall a. NullOrUndefined a
foreign import notNullOrUndefined :: forall a. a -> NullOrUndefined a
foreign import isNull :: forall a. NullOrUndefined a -> Boolean
foreign import isUndefined :: forall a. NullOrUndefined a -> Boolean
foreign import toMaybeImpl :: forall a m. Fn3 (NullOrUndefined a) m (a -> m) m

toMaybe :: forall a. NullOrUndefined a -> Maybe a
toMaybe n = runFn3 toMaybeImpl n Nothing Just
