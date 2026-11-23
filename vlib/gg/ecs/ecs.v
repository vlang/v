// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module ecs

import gg

// Entity is a generation-counted handle. Reusing an id after despawn cannot
// accidentally address the old entity because its generation changes.
pub struct Entity {
pub:
	id  u32
	gen u32
}

// World allocates entity handles and tracks which ones are alive. Components
// live in typed Storage[T] containers owned by the caller.
@[heap]
pub struct World {
pub mut:
	next_id     u32 = 1
	generations []u32
	alive       []bool
}

pub fn new_world() &World {
	return &World{}
}

// spawn allocates a new live entity.
pub fn (mut w World) spawn() Entity {
	id := w.next_id
	w.next_id++
	for w.generations.len <= int(id) {
		w.generations << 0
		w.alive << false
	}
	w.alive[id] = true
	return Entity{id, w.generations[id]}
}

// despawn invalidates an entity. Storages still holding it should drop it.
pub fn (mut w World) despawn(e Entity) {
	if !w.is_alive(e) {
		return
	}
	w.alive[e.id] = false
	w.generations[e.id]++
}

// is_alive reports whether e still refers to a live entity.
pub fn (w &World) is_alive(e Entity) bool {
	if e.id == 0 || e.id >= w.alive.len {
		return false
	}
	return w.alive[e.id] && w.generations[e.id] == e.gen
}

// count returns the number of live entities.
pub fn (w &World) count() int {
	mut n := 0
	for a in w.alive {
		if a {
			n++
		}
	}
	return n
}

// Storage is a sparse-set component container. lookups are O(1) and iteration
// over dense storage is cache friendly.
pub struct Storage[T] {
mut:
	sparse   []int // entity id -> dense slot, -1 when absent
	gens     []u32 // entity id -> generation recorded when the slot was filled
	dense    []T
	entities []Entity
}

pub fn new_storage[T]() Storage[T] {
	return Storage[T]{}
}

// add inserts or replaces a component for an entity.
pub fn (mut s Storage[T]) add(e Entity, value T) {
	for s.sparse.len <= int(e.id) {
		s.sparse << -1
		s.gens << 0
	}
	slot := s.sparse[e.id]
	if slot >= 0 {
		// A component for a different generation lives here; ignore the stale
		// handle instead of overwriting it.
		if s.gens[e.id] != e.gen {
			return
		}
		s.dense[slot] = value
		return
	}
	s.sparse[e.id] = s.dense.len
	s.gens[e.id] = e.gen
	s.dense << value
	s.entities << e
}

// get returns a component by entity, or none when absent.
pub fn (s &Storage[T]) get(e Entity) ?&T {
	if e.id >= s.sparse.len {
		return none
	}
	slot := s.sparse[e.id]
	if slot < 0 || slot >= s.dense.len || s.gens[e.id] != e.gen {
		return none
	}
	return &s.dense[slot]
}

// has reports whether an entity has this component.
pub fn (s &Storage[T]) has(e Entity) bool {
	return s.get(e) != none
}

// remove deletes a component. Missing components are a no-op.
pub fn (mut s Storage[T]) remove(e Entity) {
	if e.id >= s.sparse.len {
		return
	}
	slot := s.sparse[e.id]
	if slot < 0 || s.gens[e.id] != e.gen {
		return
	}
	last := s.dense.len - 1
	if slot != last {
		s.dense[slot] = s.dense[last]
		moved := s.entities[last]
		s.entities[slot] = moved
		s.sparse[moved.id] = slot
		s.gens[moved.id] = moved.gen
	}
	s.dense = s.dense[..last]
	s.entities = s.entities[..last]
	s.sparse[e.id] = -1
}

// len returns the number of stored components.
pub fn (s &Storage[T]) len() int {
	return s.dense.len
}

// entity_at returns the entity occupying a dense slot.
pub fn (s &Storage[T]) entity_at(slot int) ?Entity {
	if slot < 0 || slot >= s.entities.len {
		return none
	}
	return s.entities[slot]
}

// component_at returns the component at a dense slot.
pub fn (s &Storage[T]) component_at(slot int) ?&T {
	if slot < 0 || slot >= s.dense.len {
		return none
	}
	return &s.dense[slot]
}

// each calls visit for every stored component.
pub fn (s &Storage[T]) each(visit fn (Entity, &T)) {
	for i, e in s.entities {
		visit(e, &s.dense[i])
	}
}

// Transform is the common spatial component used by scenes and systems.
pub struct Transform {
pub mut:
	position gg.Vec3
	rotation gg.Vec3
	scale    gg.Vec3 = gg.Vec3{1, 1, 1}
}

// transform_mul composes child with parent (rotation is Euler, not a full
// matrix product; it is sufficient for lightweight scene hierarchies).
pub fn transform_mul(parent Transform, child Transform) Transform {
	return Transform{
		position: parent.position + child.position.scale(parent.scale.x)
		rotation: child.rotation
		scale:    gg.Vec3{parent.scale.x * child.scale.x, parent.scale.y * child.scale.y, parent.scale.z * child.scale.z}
	}
}
