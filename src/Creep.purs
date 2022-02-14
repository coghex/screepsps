module Creep where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Data.Argonaut.Decode ( printJsonDecodeError )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int ( toStringAs, decimal, fromString )
import Data.Either ( Either(..) )
import Data.Map as Map
import Foreign.Object as F
import Screeps.Types
import Screeps.Creep as Creep
import Screeps.Memory as Memory

-- | converts foreign hash table to purescript array, gets the current
--   utility score, iterate though each creep changing their behavior
--   on the possible utility increase, and changing the value at the end
processCreeps :: F.Object Creep -> Memory.MemoryGlobal -> Effect Unit
processCreeps hash mem = do
  init <- Memory.get mem "utility"
  init' <- case init of
                Left str -> do
                              log $ printJsonDecodeError str
                              pure (-1)
                Right a0 -> case a0 of
                  Nothing -> pure 0
                  Just n0 -> pure n0
  new  <- calcUtility init' array
  Memory.set mem "utility" new
  log $ "U(n-1): " <> show init' <> ", U(n): " <> show new
  where array         = F.toArrayWithKey makeArray hash
        makeArray k v = v

-- | adds together the utility of every creep after
--   they make their descisions
calcUtility :: Int -> Array Creep -> Effect Int
calcUtility util array = pure util
