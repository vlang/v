struct Entity {
	id u16
}

struct Position {
	x f64
	y f64
	z f64
}

struct Velocity {
	x f64
	y f64
	z f64
}

interface ComponentStore[T] {
	len() int
mut:
	add(Entity, T)
}

struct Store[T] {
mut:
	items []T
}

fn (mut s Store[T]) add(_ Entity, value T) {
	s.items << value
}

fn (s &Store[T]) len() int {
	return s.items.len
}

@[heap]
struct Registry {
mut:
	data map[string]ComponentStore
}

fn new_registry() &Registry {
	return &Registry{
		data: map[string]ComponentStore{}
	}
}

fn (mut r Registry) create_store[T]() {
	if T.name in r.data {
		return
	}
	r.data[T.name] = Store[T]{}
}

fn (mut r Registry) add_component[T](entity Entity, component T) {
	r.create_store[T]()
	r.data[T.name].add(entity, component)
}

fn (mut r Registry) count[T]() int {
	return r.data[T.name].len()
}

fn test_generic_interface_map_values_can_store_multiple_concrete_instantiations() {
	mut registry := new_registry()
	entity := Entity{
		id: 1
	}
	registry.add_component(entity, Position{
		x: 10
		y: 2
		z: 0
	})
	registry.add_component(entity, Position{
		x: 11
		y: 3
		z: 1
	})
	registry.add_component(entity, Velocity{
		x: 1
		y: 2
		z: 3
	})
	assert registry.count[Position]() == 2
	assert registry.count[Velocity]() == 1
}
