module ecs

import gg

fn test_entity_generations() {
	mut w := new_world()
	a := w.spawn()
	b := w.spawn()
	assert a.id != b.id
	assert w.is_alive(a) && w.is_alive(b)
	assert w.count() == 2
	w.despawn(a)
	assert !w.is_alive(a)
	assert w.count() == 1
	c := w.spawn()
	assert c.id != a.id
	assert w.is_alive(c)
	assert !w.is_alive(a)
}

fn test_storage_crud() {
	mut w := new_world()
	e1 := w.spawn()
	e2 := w.spawn()
	mut s := new_storage[int]()
	s.add(e1, 10)
	s.add(e2, 20)
	assert s.len() == 2
	v1 := s.get(e1) or { panic('missing') }
	assert *v1 == 10
	s.add(e1, 11)
	assert s.len() == 2
	v2 := s.get(e1) or { panic('missing') }
	assert *v2 == 11
	s.remove(e1)
	assert !s.has(e1) && s.len() == 1
	v3 := s.get(e2) or { panic('missing') }
	assert *v3 == 20
	s.remove(e1)
	assert s.len() == 1
}

fn test_storage_rejects_stale_generation() {
	mut w := new_world()
	e := w.spawn()
	mut s := new_storage[int]()
	s.add(e, 42)
	assert s.has(e)
	w.despawn(e)
	// A handle for the same id but a different generation must not see,
	// overwrite or remove the component stored under the original generation.
	newer := Entity{e.id, e.gen + 1}
	assert !s.has(newer)
	assert s.get(newer) == none
	s.remove(newer)
	assert s.len() == 1
	s.add(newer, 99)
	v := s.get(e) or { panic('missing') }
	assert *v == 42
	assert s.len() == 1
	assert s.has(e)
}

fn test_storage_each_and_transform() {
	mut w := new_world()
	e := w.spawn()
	mut s := new_storage[Transform]()
	s.add(e, Transform{ position: gg.Vec3{1, 2, 3} })
	mut seen := 0
	s.each(fn (entity Entity, t &Transform) {
		assert t.position.y == 2
		_ = entity
	})
	for i in 0 .. s.len() {
		seen++
	}
	assert seen == 1
	mut counter := 0
	for i in 0 .. s.len() {
		at := s.entity_at(i) or { continue }
		if at == e {
			counter++
		}
	}
	assert counter == 1
	parent := Transform{ position: gg.Vec3{1, 1, 1}, scale: gg.Vec3{2, 2, 2} }
	child := Transform{ position: gg.Vec3{1, 0, 0} }
	composed := transform_mul(parent, child)
	assert composed.position.x == 3
	assert composed.scale.x == 2
}
