module Creep where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (null, length, uncons, filter, foldl, index)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Data.Argonaut.Core ( Json, caseJsonArray, stringify, jsonEmptyString )
import Data.Argonaut.Decode ( printJsonDecodeError, JsonDecodeError, getField, decodeJson )
import Data.Argonaut.Encode ( encodeJson )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int ( toStringAs, decimal, fromString, quot )
import Data.Either ( Either(..) )
import Data.Foldable ( for_ )
import Data.Traversable ( sequence )
import Data.Map as Map
import Data.Map.Internal as MapI
import Data.Set as S
import Data.List as L
import Data.Tuple
import Foreign.Object as F
import Screeps.Types
import Screeps.Creep as Creep
import Screeps.Memory as Memory
import Screeps.Game as Game
import Screeps.Const
import Role.Harvester (preformHarvester)

-- | once each creep knows their role they come here to split off into action
preformCreeps ∷ F.Object Creep → GameGlobal
  → Memory.MemoryGlobal → Effect Unit
preformCreeps hash game mem = do
  creeps' ← Memory.get mem "creeps"
  let creeps = F.toUnfoldable $ case creeps' of
                 Left err → F.empty
                 Right c0 → c0
  preformCreepsF hash creeps
preformCreepsF ∷ F.Object Creep → Array (Tuple String (F.Object Json)) → Effect Unit
preformCreepsF _      []     = pure unit
preformCreepsF fobj creeps = do
  preformCreep fobj creep'
  preformCreepsF fobj creeps'
  where creep'  = case uncons creeps of
                    Just {head: c, tail: _}  → c
                    Nothing                  → Tuple "NULL" F.empty
        creeps' = case uncons creeps of
                    Just {head: _, tail: cs} → cs
                    Nothing                  → []
preformCreep ∷ F.Object Creep → Tuple String (F.Object Json) → Effect Unit
preformCreep _      (Tuple "NULL" val) = pure unit
preformCreep creeps (Tuple key    val) = case role of
  RoleNULL → pure unit
  RoleHarvester → do
    let creep = F.lookup key creeps
    case creep of
      Nothing → pure unit
      Just c0 → preformHarvester c0
  where role = case getField val "role" of
                  Left err → RoleNULL
                  Right r0 → r0


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
  -- this is the main algorithm, takes the old utility function and
  -- goes through each creep seeing if they can improve the situation
  else do
    -- returns creep utilities as array of ints in the same order as the index
    creeputl ← Memory.getCreepsUtl
    creeps'  ← Memory.get mem "creeps"
    creeps   ← case creeps' of
                   Left err → pure $ F.empty
                   Right c0 → pure c0
    -- global utility is sum of each creep utililty
    let utl0       = foldl (+) zero creeputl
        roleScores = map (calcRoleScore creeps) roleList
    -- new utility is just the sum of everyone's utility
    -- returns the best alternative role
        newCreeps = processCreep creeps utl0 roleScores roleList
    Memory.set mem "creeps" newCreeps
    Memory.set mem "utility" utl0
    log $ "U(n-1): " <> show utl0 <> ", U(n): "

-- | goes through each individual creep to see if they can increae utility
processCreep ∷ F.Object (F.Object Json) → Int → Array Int → Array Role → F.Object (F.Object Json)
processCreep creeps utl0 scores roles
  = F.mapWithKey (processCreepF utl0 scores roles) creeps
processCreepF ∷ Int → Array Int → Array Role → String → F.Object Json → F.Object Json
processCreepF utl0 scores roles key val0 = val2
-- this is a dumb way to do this, find the index just to index utl
-- but i dont want to use purescript tuples since i dont like them
  where val2     = F.update (\_ → newUtl) "utility" val1
        val1     = F.update (\_ → newRole) "role" val0
        alt      = bestRole scores roles 0 RoleNULL
        utl      = case getField val0 "utility" of
                      Left err → -1
                      Right u0 → u0
        rol      = case getField val0 "role" of
                      Left err → RoleNULL
                      Right r0 → r0
        ind      = findInd roles alt 0
        newUtl'  = scores `index` ind
        newUtl   = fromMaybeToMaybeJson newUtl'
        newRole  = Just $ encodeJson alt
-- | we need to always return something or foreign.update will delete things
fromMaybeToMaybeJson ∷ Maybe Int → Maybe Json
fromMaybeToMaybeJson (Nothing) = Just $ encodeJson (-1)
fromMaybeToMaybeJson (Just v)  = Just $ encodeJson (v)

-- | TODO: be a good programmer and make this Array a → a → Int → Int
findInd ∷ Array Role → Role → Int → Int
findInd []    _   n = n
findInd array val n =
  if (val == val') then n else findInd array' val (n + 1)
  where val'   = case uncons array of
                   Just {head: a, tail: _}  → a
                   Nothing                  → RoleNULL
        array' = case uncons array of
                   Just {head: _, tail: al} → al
                   Nothing                  → []


-- | finds the score for a given role for all the creeps at once
calcRoleScore ∷ F.Object (F.Object Json) → Role → Int
calcRoleScore creeps RoleNULL      = 0
calcRoleScore creeps RoleHarvester = score
  where score = 1000 `quot` (harvs + 1)
        harvs = numberOfRole RoleHarvester creeps

numberOfRole ∷ Role → F.Object (F.Object Json) → Int
numberOfRole role creeps = F.size $ F.filter roleFilter creeps

roleFilter ∷ F.Object Json → Boolean
roleFilter creep = false

bestRole ∷ Array Int → Array Role → Int → Role → Role
bestRole []     _     _     role = role
bestRole _      []    _     role = role
bestRole scores roles score role = if (score' > score) then bestRole scores' roles' score' role'
   else bestRole scores' roles' score role
   where roles'  = case uncons roles of
                     Just {head: _, tail: rs} → rs
                     Nothing → []
         role'   = case uncons roles of
                     Just {head: r, tail: _}  → r
                     Nothing → RoleNULL
         scores' = case uncons scores of
                     Just {head: _, tail: ss} → ss
                     Nothing → []
         score'  = case uncons scores of
                     Just {head: s, tail: _}  → s
                     Nothing → 0

-- | basic creep creation function
createCreep ∷ Spawn → String → Effect Unit
createCreep spawn name = do
    spawnCreep spawn [pWork,pCarry,pMove] name RoleHarvester

-- | pattern match helper function
spawnCreep ∷ Spawn → Array BodyPartType
  → String → Role → Effect Unit
spawnCreep spawn parts name RoleHarvester = do
    res ← Game.rawSpawnCreep spawn parts name RoleHarvester
    case res of
        Left  err → log $ show err
        Right str → log $ str <> " created succesfully"
spawnCreep spawn parts name RoleNULL      = pure unit
