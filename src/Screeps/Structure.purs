module Screeps.Structure where
import Prelude
import Effect (Effect)
import Screeps.Types
import Screeps.FFI ( unsafeField )

structureType ∷ ∀ a. Structure a → StructureType
structureType = unsafeField "structureType"
