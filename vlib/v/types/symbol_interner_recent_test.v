module types

fn test_symbol_interning_owns_reused_and_bounded_input_bytes() {
	mut interner := new_symbol_interner()
	mut bytes := 'module.First!'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len - 1) }
	first_id, first := interner.intern(borrowed)
	assert first == 'module.First'
	assert first.str != borrowed.str
	for index, value in 'module.Other'.bytes() {
		bytes[index] = value
	}
	second_id, second := interner.intern(borrowed)
	assert second_id != first_id
	assert second == 'module.Other'
	assert first == 'module.First'
	bytes[bytes.len - 1] = `?`
	same_id, same := interner.intern(borrowed)
	assert same_id == second_id
	assert same.str == second.str
	repeated_id, repeated := interner.intern('module.First'.clone())
	assert repeated_id == first_id
	assert repeated.str == first.str
	empty_id, empty := interner.intern('')
	assert empty_id == 0
	assert empty == ''
	assert interner.name(SymbolId(0)) == ''
	assert interner.name(SymbolId(99999)) == ''
}

fn test_symbol_interning_preserves_ids_after_growth_and_reused_slot_collisions() {
	mut interner := new_symbol_interner()
	mut bytes := 'module.Entry0'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	mut ids := []SymbolId{}
	for digit in 0 .. 10 {
		bytes[bytes.len - 1] = u8(`0` + digit)
		id, canonical := interner.intern(borrowed)
		assert canonical == 'module.Entry${digit}'
		ids << id
	}
	for n in 0 .. 4096 {
		interner.intern('growth.Symbol${n}')
	}
	for digit in 0 .. 10 {
		bytes[bytes.len - 1] = u8(`0` + digit)
		id, canonical := interner.intern(borrowed)
		assert id == ids[digit]
		assert canonical == 'module.Entry${digit}'
		assert interner.name(id) == canonical
	}
	assert interner.len() == 4106
}

fn symbol_recent_parallel_writer(mut interner SymbolInterner, names []string, ready chan bool, start chan bool) []SymbolId {
	ready <- true
	_ := <-start
	mut ids := []SymbolId{cap: names.len}
	for name in names {
		id, canonical := interner.intern(name)
		assert canonical == name
		ids << id
	}
	return ids
}

fn test_symbol_interning_remains_atomic_under_parallel_growth() {
	$if prealloc {
		// Worker arenas cannot own storage shared after their threads terminate.
		return
	}
	mut interner := new_symbol_interner()
	mut names := []string{cap: 1024}
	for n in 0 .. 1024 {
		names << 'parallel.Symbol${n}'
	}
	ready := chan bool{cap: 4}
	start := chan bool{cap: 4}
	mut workers := []thread []SymbolId{}
	for _ in 0 .. 4 {
		workers << spawn symbol_recent_parallel_writer(mut interner, names, ready, start)
	}
	for _ in 0 .. 4 {
		_ := <-ready
	}
	for _ in 0 .. 4 {
		start <- true
	}
	mut expected := []SymbolId{}
	for worker in workers {
		ids := worker.wait()
		if expected.len == 0 {
			expected = ids.clone()
		} else {
			assert ids == expected
		}
	}
	assert interner.len() == names.len
	for index, name in names {
		assert interner.name(expected[index]) == name
	}
}

fn test_symbol_interning_owns_borrowed_names_before_worker_scope_release() {
	$if prealloc {
		mut interner := new_symbol_interner()
		scope := unsafe { prealloc_scope_begin() }
		borrowed := 'worker.borrowed'.clone()
		assert unsafe { prealloc_scope_owns(scope, borrowed.str) }
		unsafe { prealloc_scope_leave(scope) }
		id, canonical := interner.intern(borrowed)
		assert !unsafe { prealloc_scope_owns(scope, canonical.str) }
		unsafe { prealloc_scope_free_after(scope) }
		repeated_id, repeated := interner.intern('worker.borrowed')
		assert repeated_id == id
		assert repeated == 'worker.borrowed'
		assert interner.name(id) == repeated
	}
}

fn test_symbol_interning_recent_id_survives_arena_promotion() {
	$if prealloc {
		mut interner := new_symbol_interner()
		interner.reserve(4)
		scope := unsafe { prealloc_scope_begin() }
		for index in 0 .. 256 {
			interner.intern('worker.promoted${index}')
		}
		assert unsafe { prealloc_scope_owns(scope, interner.names.data) }
		assert unsafe { prealloc_scope_owns(scope, interner.name(SymbolId(129)).str) }
		unsafe { prealloc_scope_leave(scope) }
		interner.promote_from(0, scope)
		assert !unsafe { prealloc_scope_owns(scope, interner.names.data) }
		assert !unsafe { prealloc_scope_owns(scope, interner.name(SymbolId(129)).str) }
		unsafe { prealloc_scope_free_after(scope) }
		for index in 0 .. 256 {
			id, canonical := interner.intern('worker.promoted${index}')
			assert id == SymbolId(index + 1)
			assert canonical == 'worker.promoted${index}'
			assert interner.name(id) == canonical
		}
	}
}
