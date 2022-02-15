module Creep where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (null, length,uncons)
import Data.Array.Partial (head, tail)
import Data.Tuple (Tuple, snd)
import Partial.Unsafe (unsafePartial)
import Data.Argonaut.Decode ( printJsonDecodeError )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int ( toStringAs, decimal, fromString )
import Data.Either ( Either(..) )
import Data.Tuple (Tuple)
import Data.Map as Map
import Foreign.Object as F
import Screeps.Types
import Screeps.Creep as Creep
import Screeps.Memory as Memory
import Screeps.Game as Game
import Screeps.Const

-- | converts foreign hash table to purescript array, gets the current
--   utility score, iterate though each creep changing their behavior
--   on the possible utility increase, and changing the value at the end
processCreeps :: F.Object Creep -> GameGlobal -> Memory.MemoryGlobal -> Effect Unit
processCreeps hash game mem = do
  -- before making any descisions we check to see if we even have creeps,
  -- the algorithm needs at least one
  if F.isEmpty hash then do
    log "creating initial creep"
    let spawnslist = Game.spawns game
        -- TODO: keep track of spawns, get rid of this lookup
        spawn1     = F.lookup "Spawn1" spawnslist
    case spawn1 of
      Nothing -> log "fatal error"
      Just s1 -> createCreep s1 "Creep1"
  else do
    init  <- Memory.get mem "utility"
    init' <- case init of
                  Left str -> do
                                log $ printJsonDecodeError str
                                pure (-1)
                  Right a0 -> case a0 of
                    Nothing -> pure 0
                    Just n0 -> pure n0
    creeputl  <- Memory.getCreepsUtl
    new       <- calcUtility init' 0 array creeputl
    Memory.set mem "utility" new
    log $ "U(n-1): " <> show init' <> ", U(n): " <> show new
    where array             = F.toArrayWithKey makeArray hash
          makeArray     _ v = v


-- | adds together the utility of every creep after
--   they make their descisions
calcUtility :: Int -> Int -> Array Creep -> Array Int -> Effect Int
calcUtility util n []    creepmem = pure n
calcUtility util n array []       = pure n
calcUtility util n array creepmem = calcUtility util (n + n') array creepmem'
  where creepmem' = case (uncons creepmem) of
               Just {head: _, tail: cs} -> cs
               Nothing -> []
        n'        = case (uncons creepmem) of
               Just {head: c, tail: _}  -> c
               Nothing -> 0
               

-- | basic creep creation function
createCreep :: Spawn -> String -> Effect Unit
createCreep spawn name = do
    spawnCreep spawn [pWork,pCarry,pMove] name RoleHarvester

spawnCreep :: Spawn -> Array BodyPartType -> String -> Role -> Effect Unit
spawnCreep spawn parts name RoleHarvester = do
    res <- Game.rawSpawnCreep spawn parts name "harv"
    case res of
        Left err -> log $ show err
        Right str -> log $ str <> " created succesfully"
spawnCreep spawn parts name RoleNULL      = pure unit
