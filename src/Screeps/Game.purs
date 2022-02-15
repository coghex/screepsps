module Screeps.Game where
import Prelude
import Foreign.Object as F
import Effect (Effect)
import Screeps.Types
import Screeps.FFI (runThisEffFn0, runThisFn1, unsafeField, toMaybe)
import Data.Maybe ( Maybe )
import Data.Either ( Either(..) )
import Data.Map as Map

foreign import getGameGlobal :: Effect GameGlobal
foreign import createCreepImpl :: Spawn -> Array BodyPartType
    -> (ReturnCode -> Either ReturnCode String)
    -> (String -> Either ReturnCode String)
    -> Effect (Either ReturnCode String)

type Cpu = { limit :: Int
           , tickLimit :: Int
           , bucket :: Int }

cpu :: GameGlobal -> Cpu
cpu = unsafeField "cpu"
spawns :: GameGlobal -> F.Object Spawn
spawns = unsafeField "spawns"

creeps :: GameGlobal -> F.Object Creep
creeps = unsafeField "creeps"

getUsed :: GameGlobal -> Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)

getObjectById :: forall a. GameGlobal -> Id a -> Maybe a
getObjectById game id = toMaybe (runThisFn1 "getObjectById" game id)

rawSpawnCreep ∷ Spawn -> Array BodyPartType -> String -> String -> Effect (Either ReturnCode String)
rawSpawnCreep spawn parts name role = createCreepImpl spawn parts Left Right
