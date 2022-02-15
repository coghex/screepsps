module Creep where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (null, length, uncons, filter)
import Data.Array.Partial (head, tail)
import Data.Tuple (Tuple, snd)
import Partial.Unsafe (unsafePartial)
import Data.Argonaut.Decode ( printJsonDecodeError, JsonDecodeError )
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
processCreeps ∷ F.Object Creep → GameGlobal
  → Memory.MemoryGlobal → Effect Unit
processCreeps hash game mem = do
  -- before making any descisions we check to see if we even
  -- have creeps, the algorithm needs at least one
  if F.isEmpty hash then do
    log "creating initial creep"
    let spawnslist = Game.spawns game
        -- TODO: keep track of spawns, get rid of this lookup
        spawn1     = F.lookup "Spawn1" spawnslist
    case spawn1 of
    -- TODO: figure out if spawn1 is always present 
      Nothing → log "fatal error: no Spawn1"
      -- first creep will get a random name, creep1 is the index
      Just s1 → createCreep s1 "Creep1"
  else do
  -- this is the main algorithm, takes the old utility function and
  -- goes through each creep seeing if they can improve the situation
--    init  ← Memory.get mem "utility"
--    init' ← case init of
--               Left str → do
--                             log $ printJsonDecodeError str
--                             pure (-1)
--               Right a0 → case a0 of
--                 Nothing → pure 0
--                 Just n0 → pure n0
    -- returns creep utilities as array of ints in the same order as the index
    creeputl ← Memory.getCreepsUtl
    -- new utility is just the sum of everyone's utility
    new      ← calcUtilities 0 array creeputl
--    possible ← maximiseUtility 0 new creeputl
    Memory.set mem "utility" new
--    log $ "U(n-1): " <> show init' <> ", U(n): " <> show new
    where array             = F.toArrayWithKey processCreep hash

-- | handles the logic for an individual creep, pure for now to keep things fast
--   we could use this handy foreign lib function to apply a funciton to each
--   member of the creeps hash then convert it to an array without keys
--   but i cant figure out how to return the data yet
processCreep ∷ String → Creep → Creep
processCreep _ v = v

-- | adds together the utility of every creep after
--   they make their descisions
calcUtilities ∷ Int → Array Creep → Array Int → Effect Int
calcUtilities n []    creepmem = pure n
calcUtilities n array []       = pure n
calcUtilities n array creepmem
  = calcUtilities (n + n') array creepmem'
  where creepmem' = case (uncons creepmem) of
                      Just {head: _, tail: cs} → cs
                      Nothing → []
        n'        = case (uncons creepmem) of
                      Just {head: c, tail: _}  → c
                      Nothing → 0
               
-- | basic creep creation function
createCreep ∷ Spawn → String → Effect Unit
createCreep spawn name = do
    spawnCreep spawn [pWork,pCarry,pMove] name RoleHarvester

-- | pattern match helper function
spawnCreep ∷ Spawn → Array BodyPartType
  → String → Role → Effect Unit
spawnCreep spawn parts name RoleHarvester = do
    res ← Game.rawSpawnCreep spawn parts name "harv"
    case res of
        Left  err → log $ show err
        Right str → log $ str <> " created succesfully"
spawnCreep spawn parts name RoleNULL      = pure unit

-- -- | calcualtes all utiliteies for all hypothetical alternatives
-- maximiseUtilities ∷ Int → Int → Array Int → Array Creep → Int
-- maximiseUtilities n utl0 utls creeps = utl1
--   where best = calcAlternatives (creeps[0].memory.role) roleList utls creeps
--         utl1 = utl0
-- 
-- -- | alternatives for each possible role
-- calcAlternatives ∷ Role → Array Role → Array Int → Array Creep → Role
-- calcAlternatives current roles array creepmem = roles[best]
--   where best = max (calcAlternative current roles array creepmem)
-- calcAlternative ∷ Role → Array Role → Array Int → Array Creep → Array Int
-- calcAlternative _       []    _     _        = []
-- calcAlternative current roles array creepmem
--   = score <> calcAlternative roles' array creepmem
--   where roles' = case uncons roles of
--                    Just {head: _, tail: rs} → rs
--                    Nothing → []
--         score  = case uncons roles of
--                    Just {head: r, tail: _}  → calcAlternativeScore r creepmem
--                    Nothing → []
-- calcAlternativeScore ∷ Role → Array Creep → Int
-- calcAlternativeScore role creepmem = 0

-- | this is the main utility calculation function,
--   it goes though and calculates the utility for every creep
calcUtility ∷ Array Role → Array Int
calcUtility roles = calcUtilityF roles nHarv nNULL
  where nHarv = length $ filter ((==) RoleHarvester) roles
        nNULL = length $ filter ((==) RoleNULL)      roles
calcUtilityF ∷ Array Role → Int → Int → Array Int
calcUtilityF []    _     _     = []
calcUtilityF roles nHarv nNULL = cost <> calcUtilityF rolestail nHarv nNULL
  where rolestail = case uncons roles of
                 Just {head: _, tail: rt} → rt
                 Nothing                  → []
        cost      = case uncons roles of
                 Just {head: r, tail: _}  → [costOfRole r]
                 Nothing                  → [-1]
costOfRole ∷ Role → Int
costOfRole r = 0
