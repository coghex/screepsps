module Screeps.Types where
import Prelude

foreign import data GameGlobal :: Type
foreign import data RawRoomObject :: Type -> Type
foreign import data RawStructure :: Type -> Type
foreign import data RawOwnedStructure :: Type -> Type
foreign import data RawSpawn :: Type

type RoomObject a = RawRoomObject a
type Structure a = RoomObject (RawStructure a)
type OwnedStructure a = Structure (RawOwnedStructure a)
type Spawn = OwnedStructure RawSpawn
