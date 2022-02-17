module Screeps.Types where
import Prelude
import Data.Eq
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Generic.Rep ( class Generic )
import Data.Eq.Generic ( genericEq )
import Data.Show ( class Show )
import Data.Show.Generic ( genericShow )
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..), getField)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)

foreign import data GameGlobal :: Type
foreign import data RawRoomObject :: Type -> Type
foreign import data Room :: Type
foreign import data WorldMap :: Type
foreign import data RawStructure :: Type -> Type
foreign import data RawOwnedStructure :: Type -> Type
foreign import data RawConstructionSite :: Type
foreign import data RawSpawn :: Type
foreign import data RawTower :: Type
foreign import data RawCreep :: Type
foreign import data RawMineral :: Type
foreign import data RawResource :: Type
foreign import data RawSource :: Type
foreign import data RawStorage :: Type
foreign import data RawFlag :: Type
foreign import data RawNuke :: Type
foreign import data RoomPosition :: Type
foreign import data Store ∷ Type

type RoomObject     a = RawRoomObject  a
type Structure      a = RoomObject     (RawStructure a)
type OwnedStructure a = Structure      (RawOwnedStructure a)
type ConstructionSite = RoomObject     RawConstructionSite
type Storage          = OwnedStructure RawStorage
type Spawn            = OwnedStructure RawSpawn
type Tower            = OwnedStructure RawTower
type Creep            = RoomObject     RawCreep
type Mineral          = RoomObject     RawMineral
type Resource         = RoomObject     RawResource
type Source           = RoomObject     RawSource
type Flag             = RoomObject     RawFlag
type Nuke             = RoomObject     RawNuke

type Path = Array PathStep

type PathStep =
  { x :: Int
  , y :: Int
  , dx :: Number
  , dy :: Number
  , direction :: Direction }
  
newtype Id a = Id String
derive instance genericId :: Generic (Id a) _
instance eqId :: Eq (Id a) where eq = genericEq
instance showId :: Show (Id a) where show = genericShow
instance decodeJsonId :: DecodeJson (Id a) where decodeJson = genericDecodeJson
instance encodeJsonId :: EncodeJson (Id a) where encodeJson = genericEncodeJson

type BodyPart =
  { boost :: Maybe String
  , type  :: BodyPartType
  , hits  :: Int }

type MoveOptions = PathOptions
  ( reusePath :: Maybe Int
  , serializeMemory :: Maybe Boolean
  , noPathFinding :: Maybe Boolean )

type PathOptions o =
  { ignoreCreeps :: Maybe Boolean
  , ignoreDestructibleStructures :: Maybe Boolean
  , ignoreRoads :: Maybe Boolean
  , ignore :: Maybe (Array RoomPosition)
  , avoid :: Maybe (Array RoomPosition)
  , maxOps :: Maybe Int
  , heuristicWeight :: Maybe Number
  , serialize :: Maybe Boolean
  , maxRooms :: Maybe Int
  | o }

moveOpts :: MoveOptions
moveOpts =
  { ignoreCreeps: Nothing
  , ignoreDestructibleStructures: Nothing
  , ignoreRoads: Nothing
  , ignore: Nothing
  , avoid: Nothing
  , maxOps: Nothing
  , heuristicWeight: Nothing
  , serialize: Nothing
  , maxRooms: Nothing
  , reusePath: Nothing
  , serializeMemory: Nothing
  , noPathFinding: Nothing }

pathOpts :: PathOptions ()
pathOpts =
  { ignoreCreeps: Nothing
  , ignoreDestructibleStructures: Nothing
  , ignoreRoads: Nothing
  , ignore: Nothing
  , avoid: Nothing
  , maxOps: Nothing
  , heuristicWeight: Nothing
  , serialize: Nothing
  , maxRooms: Nothing }

newtype BodyPartType = BodyPartType String
derive instance genericBodyPartType :: Generic BodyPartType _
instance eqBodyPartType :: Eq BodyPartType where eq = genericEq
instance showBodyPartType :: Show BodyPartType where show = genericShow

newtype ResourceType = ResourceType String
derive instance genericResourceType :: Generic ResourceType _
instance eqResourceType :: Eq ResourceType where eq = genericEq
instance showResourceType :: Show ResourceType where
  show (ResourceType s) = s

newtype ReturnCode = ReturnCode Int
derive instance genericReturnCode :: Generic ReturnCode _
instance eqReturnCode :: Eq ReturnCode where eq = genericEq
instance showReturnCode :: Show ReturnCode where
  show (ReturnCode n) = show n

newtype StructureType = StructureType String
derive instance genericStructureType :: Generic StructureType _
instance eqStructureType :: Eq StructureType where eq = genericEq
instance showStructureType :: Show StructureType where show = genericShow

newtype Direction = Direction Int
derive instance genericDirection :: Generic Direction _
instance eqDirection :: Eq Direction where eq = genericEq
instance showDirection :: Show Direction where show = genericShow

newtype TerrainMask = TerrainMask Int
derive instance genericTerrainMask :: Generic TerrainMask _
instance eqTerrainMask :: Eq TerrainMask where eq = genericEq
instance showTerrainMask :: Show TerrainMask where show = genericShow

newtype Terrain = Terrain String
derive instance genericTerrain :: Generic Terrain _
instance eqTerrain :: Eq Terrain where eq = genericEq
instance showTerrain :: Show Terrain
  where show (Terrain s) = s

data TargetPosition a =
  TargetPt Int Int |
  TargetObj (RoomObject a) |
  TargetPos RoomPosition
type FilterFn a = a -> Boolean
newtype FindType a = FindType Int

-- | creep types are generic, irrelevant of job or role
data CreepType = CreepDrone | CreepNULL
-- | creeps can take on many roles, depending on what type they are
data Role = RoleIdle | RoleHarvester | RoleNULL
instance showRole ∷ Show Role where
  show RoleHarvester = "RoleHarvester"
  show RoleIdle      = "RoleIdle"
  show RoleNULL      = "RoleNULL"
instance eqRoles ∷ Eq Role where
  eq RoleNULL      RoleNULL      = true
  eq RoleIdle      RoleIdle      = true
  eq RoleHarvester RoleHarvester = true
  eq _             RoleNULL      = false
  eq RoleNULL      _             = false
  eq _             _             = false
roleList = [RoleIdle, RoleHarvester, RoleNULL] ∷ Array Role
instance encodeRole :: EncodeJson Role where
  encodeJson RoleIdle      = encodeJson "RoleIdle"
  encodeJson RoleHarvester = encodeJson "RoleHarvester"
  encodeJson RoleNULL      = encodeJson "RoleNULL"
instance decodeRole :: DecodeJson Role where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Role:") (roleFromStr string)
roleFromStr :: String -> Maybe Role
roleFromStr "RoleHarvester" = Just RoleHarvester
roleFromStr "RoleIdle"      = Just RoleIdle
roleFromStr "RoleNULL"      = Just RoleNULL
roleFromStr _               = Nothing
-- | jobs are like temporary roles
data Job = JobBuild | JobNULL
-- | the main loop control variable allow some control over state
--   encoded straight into the json memory so we write some instances
data LoopStatus = LoopGo | LoopStop | LoopNULL
instance encodeLoopStatus :: EncodeJson LoopStatus where
    encodeJson LoopGo = encodeJson "loopGo"
    encodeJson LoopStop = encodeJson "loopStop"
    encodeJson LoopNULL = encodeJson "loopNULL"
instance decodeLoopStatus :: DecodeJson LoopStatus where
    decodeJson json = do
      string <- decodeJson json
      note (TypeMismatch "LoopStatus:") (lsFromStr string)
lsFromStr :: String -> Maybe LoopStatus
lsFromStr "loopGo"   = Just LoopGo
lsFromStr "loopStop" = Just LoopStop
lsFromStr "loopNULL" = Just LoopNULL
lsFromStr _          = Nothing
