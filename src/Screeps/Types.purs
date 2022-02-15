module Screeps.Types where
import Prelude
import Data.Maybe (Maybe(Nothing))
import Data.Generic.Rep ( class Generic )
import Data.Eq.Generic ( genericEq )
import Data.Show.Generic ( genericShow )
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)

foreign import data GameGlobal :: Type
foreign import data RawRoomObject :: Type -> Type
foreign import data RawStructure :: Type -> Type
foreign import data RawOwnedStructure :: Type -> Type
foreign import data RawConstructionSite :: Type
foreign import data RawSpawn :: Type
foreign import data RawTower :: Type
foreign import data RawCreep :: Type
foreign import data RawMineral :: Type
foreign import data RawResource :: Type
foreign import data RawSource :: Type
foreign import data RoomPosition :: Type

type RoomObject     a = RawRoomObject  a
type Structure      a = RoomObject     (RawStructure a)
type OwnedStructure a = Structure      (RawOwnedStructure a)
type ConstructionSite = RoomObject     RawConstructionSite
type Spawn            = OwnedStructure RawSpawn
type Tower            = OwnedStructure RawTower
type Creep            = RoomObject     RawCreep
type Mineral          = RoomObject     RawMineral
type Resource         = RoomObject     RawResource
type Source           = RoomObject     RawSource

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

newtype Direction = Direction Int
derive instance genericDirection :: Generic Direction _
instance eqDirection :: Eq Direction where eq = genericEq
instance showDirection :: Show Direction where show = genericShow

data TargetPosition a =
  TargetPt Int Int |
  TargetObj (RoomObject a) |
  TargetPos RoomPosition

data Role = RoleHarvester | RoleNULL
