module Screeps.Creep where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Screeps.Types
import Screeps.FFI ( unsafeField, runThisFn1
                   , runThisEffFn1, runThisEffFn2
                   , runThisEffFn3, toMaybe )
foreign import data CreepCargo :: Type
foreign import totalAmtCarrying :: Creep -> Int

body :: Creep -> Array BodyPart
body creep = unsafeField "body" creep

carry :: Creep -> CreepCargo
carry = unsafeField "carry"

amtCarrying :: Creep -> ResourceType -> Int
amtCarrying creep res = unsafeField (show res) $ carry creep

carryCapacity :: Creep -> Int
carryCapacity = unsafeField "carryCapacity"

fatigue :: Creep -> Int
fatigue = unsafeField "fatigue"

hits :: Creep -> Int
hits = unsafeField "hits"

hitsMax :: Creep -> Int
hitsMax = unsafeField "hitsMax"

getId :: Creep -> Id Creep
getId = unsafeField "id"

getIdAsStr :: Creep -> String
getIdAsStr = unsafeField "id"

my :: Creep -> Boolean
my = unsafeField "my"

name :: Creep -> String
name = unsafeField "name"

owner :: Creep -> { username :: String }
owner = unsafeField "owner"

saying :: Creep -> Maybe String
saying c = toMaybe $ unsafeField "saying" c

spawning :: Creep -> Boolean
spawning = unsafeField "spawning"

ticksToLive :: Creep -> Int
ticksToLive = unsafeField "ticksToLive"

attackCreep :: Creep -> Creep -> Effect ReturnCode
attackCreep = runThisEffFn1 "attack"

attackStructure :: forall a. Creep → Structure a -> Effect ReturnCode
attackStructure = runThisEffFn1 "attack"

attackController :: forall a. Creep -> Structure a -> Effect ReturnCode
attackController = runThisEffFn1 "attackController"

build :: Creep -> ConstructionSite -> Effect ReturnCode
build = runThisEffFn1 "build"

cancelOrder :: Creep -> String -> Effect ReturnCode
cancelOrder = runThisEffFn1 "cancelOrder"

claimController :: forall a. Creep -> Structure a -> Effect ReturnCode
claimController = runThisEffFn1 "claimController"

dismantle :: forall a. Creep -> Structure a -> Effect ReturnCode
dismantle = runThisEffFn1 "dismantle"

drop :: Creep -> ResourceType -> Effect ReturnCode
drop = runThisEffFn1 "drop"

dropAmt :: Creep -> ResourceType -> Int -> Effect ReturnCode
dropAmt = runThisEffFn2 "drop"

getActiveBodyparts :: Creep -> BodyPartType -> Int
getActiveBodyparts = runThisFn1 "getActiveBodyparts"

harvestSource :: Creep -> Source -> Effect ReturnCode
harvestSource = runThisEffFn1 "harvest"

harvestMineral :: Creep -> Mineral -> Effect ReturnCode
harvestMineral = runThisEffFn1 "harvest"

heal :: Creep -> Creep -> Effect ReturnCode
heal = runThisEffFn1 "heal"

move :: Creep -> Direction -> Effect ReturnCode
move = runThisEffFn1 "move"

moveByPath :: Creep -> Path -> Effect ReturnCode
moveByPath = runThisEffFn1 "moveByPath"

moveTo :: forall a. Creep -> TargetPosition a -> Effect ReturnCode
moveTo creep (TargetPt  x y) = runThisEffFn2 "moveTo" creep x y
moveTo creep (TargetPos pos) = runThisEffFn1 "moveTo" creep pos
moveTo creep (TargetObj obj) = runThisEffFn1 "moveTo" creep obj

transferToCreep :: Creep -> Creep -> ResourceType -> Int -> Effect ReturnCode
transferToCreep = runThisEffFn3 "transfer"

transferToStructure :: forall a. Creep -> Structure a -> ResourceType -> Effect ReturnCode
transferToStructure = runThisEffFn2 "transfer"

transferAmtToStructure :: forall a. Creep -> Structure a -> ResourceType -> Int -> Effect ReturnCode
transferAmtToStructure = runThisEffFn3 "transfer"

creepStore ∷ Creep → Store
creepStore = unsafeField "store"
