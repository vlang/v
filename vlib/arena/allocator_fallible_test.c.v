import arena

fn fallible_allocate(allocator arena.Allocator, size int) ?&u8 {
	return unsafe { allocator.try_calloc(size) }
}

fn test_fallible_zero_and_invalid_sizes_do_not_allocate() {
	mut storage := arena.new(chunk_size: 128)
	defer { storage.free() }
	for allocator in [storage.allocator(), arena.heap_allocator()] {
		if pointer := fallible_allocate(allocator, 0) {
			assert isnil(pointer)
		} else {
			assert false, 'zero is a successful empty allocation'
		}
		if _ := fallible_allocate(allocator, -1) {
			assert false
		}
		if _ := fallible_allocate(allocator, max_int) {
			assert false
		}
	}
	assert storage.used() == 0
	assert storage.capacity() == 0
}

fn test_fallible_allocation_preserves_contents_and_arena_after_failure() {
	mut storage := arena.new(chunk_size: 128)
	defer { storage.free() }
	allocator := storage.allocator()
	pointer := fallible_allocate(allocator, 64) or { panic('initial allocation failed') }
	assert !isnil(pointer)
	assert storage.owns(pointer)
	unsafe {
		for i in 0 .. 64 {
			assert pointer[i] == 0
			pointer[i] = u8(i + 1)
		}
	}
	used, capacity := storage.used(), storage.capacity()
	if _ := fallible_allocate(allocator, max_int) {
		assert false
	}
	assert storage.used() == used
	assert storage.capacity() == capacity
	unsafe {
		for i in 0 .. 64 {
			assert pointer[i] == u8(i + 1)
		}
	}
	next := fallible_allocate(allocator, 512) or { panic('arena not reusable') }
	assert storage.owns(next)
	unsafe {
		for i in 0 .. 512 {
			assert next[i] == 0
		}
	}
	storage.push()
	assert storage.is_current()
	storage.pop()
	storage.reset()
	assert storage.used() == 0
	again := fallible_allocate(allocator, 64) or { panic('arena not reusable after reset') }
	unsafe {
		for i in 0 .. 64 {
			assert again[i] == 0
		}
	}
}

fn test_fallible_selected_allocator_does_not_change_current_or_fall_back() {
	mut parent := arena.new(chunk_size: 128)
	mut child := arena.new(chunk_size: 128)
	parent.push()
	selected := parent.allocator()
	child.push()
	before := child.used()
	pointer := fallible_allocate(selected, 32) or { panic('ancestor allocation failed') }
	assert parent.owns(pointer)
	assert !child.owns(pointer)
	assert child.is_current()
	assert child.used() == before
	if _ := fallible_allocate(selected, max_int) {
		assert false
	}
	assert child.is_current()
	assert child.used() == before
	heap := fallible_allocate(arena.heap_allocator(), 32) or { panic('heap allocation failed') }
	assert !parent.owns(heap)
	assert !child.owns(heap)
	assert child.used() == before
	unsafe {
		for i in 0 .. 32 {
			assert heap[i] == 0
		}
		free(heap)
	}
	child.pop()
	parent.pop()
	child.free()
	parent.free()
}

fn test_fallible_oversized_chunk_layout_fails_without_corrupting_registry() {
	mut impossible := arena.new(chunk_size: max_int)
	if _ := fallible_allocate(impossible.allocator(), 8) {
		assert false
	}
	assert impossible.used() == 0
	assert impossible.capacity() == 0
	impossible.push()
	impossible.pop()
	impossible.free()
	mut normal := arena.new(chunk_size: 128)
	defer { normal.free() }
	pointer := fallible_allocate(normal.allocator(), 8) or { panic('registry left locked') }
	assert normal.owns(pointer)
}
