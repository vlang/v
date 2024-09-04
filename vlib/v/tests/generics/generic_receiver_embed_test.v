pub type Entity = u64
type ComponentType = u16

pub struct Context {
pub mut:
	ecs &Ecs
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
pub struct EntityManager {
mut:
	living_entity_count u64
}

fn (mut self EntityManager) create_entity() Entity {
	entity := self.living_entity_count
	self.living_entity_count++
	return entity
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

pub interface ISystem {
	ecs &Ecs
mut:
	init()
}

pub struct System {
pub mut:
	ecs &Ecs
}

pub fn (mut self System) init() {
}

pub struct SystemManager {
mut:
	system_array map[string]ISystem = {}
}

fn (mut self SystemManager) add_system[T]() &T {
	mut t := T{}
	self.system_array[typeof[T]().name] = t
	return &t
}

////////////////////////////////////////////////////////////////////////////

@[heap]
pub struct Ecs {
mut:
	entity_manager EntityManager
	system_manager SystemManager
pub mut:
	root_entity Entity
}

pub fn Ecs.new() &Ecs {
	mut ecs := &Ecs{}
	ecs.root_entity = ecs.create_entity()
	return ecs
}

pub fn (mut self Ecs) create_entity() Entity {
	return self.entity_manager.create_entity()
}

pub fn (mut self Ecs) add_system[T]() {
	mut system := self.system_manager.add_system[T]()
	system.ecs = &self
	system.init()
}

////////////////////////////////////////////////////////////////////////////////////////////////////////

struct NetworkUdpServerComponent {
}

struct NetworkServerSystemUdp {
	System
}

pub struct ConfigSystem {
	System
}

struct NetworkServerSystemUdpExt {
	NetworkServerSystemUdp
}

fn test_main() {
	mut ecs := Ecs.new()
	mut root_entity := ecs.create_entity()

	ecs.add_system[ConfigSystem]()
	ecs.add_system[NetworkServerSystemUdpExt]()

	assert true
}
