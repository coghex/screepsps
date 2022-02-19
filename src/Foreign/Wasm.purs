module Foreign.Wasm where
-- ^ all of the web assembly from C functions are here
import Prelude
import Effect (Effect)

foreign import calcUtility ∷ Int → Int
