module Screeps.Const where
import Prelude
import Screeps.Types


foreign import ok :: ReturnCode
foreign import err_not_owner :: ReturnCode
foreign import err_no_path :: ReturnCode
foreign import err_name_exists :: ReturnCode
foreign import err_busy :: ReturnCode
foreign import err_not_found :: ReturnCode
foreign import err_not_enough_energy :: ReturnCode
foreign import err_not_enough_resources :: ReturnCode
foreign import err_invalid_target :: ReturnCode
foreign import err_full :: ReturnCode
foreign import err_not_in_range :: ReturnCode
foreign import err_invalid_args :: ReturnCode
foreign import err_tired :: ReturnCode
foreign import err_no_bodypart :: ReturnCode
foreign import err_not_enough_extensions :: ReturnCode
foreign import err_rcl_not_enough :: ReturnCode
foreign import err_gcl_not_enough :: ReturnCode

foreign import pMove :: BodyPartType
foreign import pWork :: BodyPartType
foreign import pCarry :: BodyPartType

foreign import find_exit_top :: FindType RoomPosition
foreign import find_exit_right :: FindType RoomPosition
foreign import find_exit_bottom :: FindType RoomPosition
foreign import find_exit_left :: FindType RoomPosition
foreign import find_exit :: FindType RoomPosition
foreign import find_creeps :: FindType Creep
foreign import find_my_creeps :: FindType Creep
foreign import find_hostile_creeps :: FindType Creep
foreign import find_sources_active :: FindType Source
foreign import find_sources :: FindType Source
foreign import find_dropped_energy :: FindType Resource
foreign import find_dropped_resources :: FindType Resource
foreign import find_structures :: FindType (Structure Unit)
foreign import find_my_structures :: forall a. FindType (Structure a)
foreign import find_hostile_structures :: FindType (Structure Unit)
foreign import find_flags :: FindType Flag
foreign import find_construction_sites :: FindType ConstructionSite
foreign import find_my_spawns :: FindType Spawn
foreign import find_hostile_spawns :: FindType Spawn
foreign import find_my_construction_sites :: FindType ConstructionSite
foreign import find_hostile_construction_sites :: FindType ConstructionSite
foreign import find_minerals :: FindType Mineral
foreign import find_nukes :: FindType Nuke

foreign import structure_spawn :: StructureType
foreign import structure_extension :: StructureType
foreign import structure_road :: StructureType
foreign import structure_wall :: StructureType
foreign import structure_rampart :: StructureType
foreign import structure_keeper_lair :: StructureType
foreign import structure_portal :: StructureType
foreign import structure_controller :: StructureType
foreign import structure_link :: StructureType
foreign import structure_storage :: StructureType
foreign import structure_tower :: StructureType
foreign import structure_observer :: StructureType
foreign import structure_power_bank :: StructureType
foreign import structure_power_spawn :: StructureType
foreign import structure_extractor :: StructureType
foreign import structure_lab :: StructureType
foreign import structure_terminal :: StructureType
foreign import structure_container :: StructureType
foreign import structure_nuker :: StructureType
