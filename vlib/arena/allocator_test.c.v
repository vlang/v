import arena

fn check_zeroed_and_fill(address &u8, size int, alignment int) {
	assert uint(address) % uint(alignment) == 0
	for i in 0 .. size {
		assert unsafe { address[i] } == 0
		unsafe { address[i] = 0xff }
	}
}

fn test_explicit_heap_allocation_keeps_current_arena_and_lives_after_release() {
	heap := arena.heap_allocator()
	assert heap.is_current()
	mut storage := arena.new()
	storage.push()
	current := arena.current_allocator()
	assert current.is_current()
	assert !heap.is_current()
	used := storage.used()
	address := unsafe { heap.calloc(128) }
	used_after := storage.used()
	assert used_after == used
	assert storage.is_current()
	assert !storage.owns(address)
	check_zeroed_and_fill(address, 128, int(sizeof(voidptr)))
	from_arena := unsafe { current.calloc(8) }
	assert storage.owns(from_arena)
	storage.pop()
	assert !current.is_current()
	storage.free()
	assert heap.is_current()
	for i in 0 .. 128 {
		assert unsafe { address[i] } == 0xff
	}
	unsafe { free(address) }
}

fn test_explicit_allocation_in_inactive_arena_does_not_push_it() {
	mut storage := arena.new(chunk_size: 128)
	defer { storage.free() }
	allocator := storage.allocator()
	assert !allocator.is_current()
	for _ in 0 .. 4 {
		for size in [1, 3, 8, 16, 17, 31, 32, 127, 128, 1000] {
			address := unsafe { allocator.calloc(size) }
			assert !arena.active()
			assert storage.owns(address)
			check_zeroed_and_fill(address, size, 16)
		}
		storage.reset()
		assert storage.used() == 0
	}
}

fn test_explicit_allocation_in_parent_arena_preserves_child_allocator() {
	mut parent := arena.new()
	mut child := arena.new()
	defer {
		child.free()
		parent.free()
	}
	parent.push()
	parent_allocator := arena.current_allocator()
	child.push()
	child_used := child.used()
	address := unsafe { parent_allocator.calloc(64) }
	assert child.used() == child_used
	assert child.is_current()
	assert !parent_allocator.is_current()
	assert parent.owns(address)
	assert !child.owns(address)
	check_zeroed_and_fill(address, 64, 16)
	child.pop()
	assert parent_allocator.is_current()
	parent.pop()
	child.free()
	for i in 0 .. 64 {
		assert unsafe { address[i] } == 0xff
	}
}

fn test_explicit_allocation_in_inactive_sibling_preserves_current_allocator() {
	mut target := arena.new(chunk_size: 128)
	mut active := arena.new(chunk_size: 128)
	defer {
		active.free()
		target.free()
	}
	target_allocator := target.allocator()
	active.push()
	active_allocator := arena.current_allocator()
	active_used := active.used()
	address := unsafe { target_allocator.calloc(64) }
	assert active.used() == active_used
	assert active.is_current()
	assert active_allocator.is_current()
	assert !target_allocator.is_current()
	assert target.owns(address)
	assert !active.owns(address)
	check_zeroed_and_fill(address, 64, 16)
	target_used := target.used()
	ordinary := unsafe { vcalloc(16) }
	assert active.owns(ordinary)
	assert !target.owns(ordinary)
	assert target.used() == target_used
	active.pop()
	active.free()
	for i in 0 .. 64 {
		assert unsafe { address[i] } == 0xff
	}
}

fn test_explicit_zero_allocation_uses_no_storage_or_allocator_state() {
	mut storage := arena.new()
	defer { storage.free() }
	allocator := storage.allocator()
	heap := arena.heap_allocator()
	address := unsafe { allocator.calloc(0) }
	heap_address := unsafe { heap.calloc(0) }
	assert address == unsafe { nil }
	assert heap_address == unsafe { nil }
	assert storage.capacity() == 0
	assert storage.used() == 0
	assert !arena.active()
}

fn allocate_on_worker(allocator arena.Allocator) &u8 {
	assert !arena.active()
	address := unsafe { allocator.calloc(32) }
	check_zeroed_and_fill(address, 32, 16)
	assert !arena.active()
	return address
}

fn test_explicit_inactive_arena_allocator_can_be_used_on_another_thread() {
	mut storage := arena.new()
	defer { storage.free() }
	allocator := storage.allocator()
	worker := spawn allocate_on_worker(allocator)
	address := worker.wait()
	assert storage.owns(address)
	assert !arena.active()
	for i in 0 .. 32 {
		assert unsafe { address[i] } == 0xff
	}
}

struct AllocatorPayload {
	text string
}

fn store_heap_payload(allocator arena.Allocator) &voidptr {
	// Call while no arena is pushed: the payload uses V's ordinary allocator,
	// while its sole retained pointer can live in an explicit arena allocation.
	assert !arena.active()
	slot := unsafe { &voidptr(allocator.calloc(int(sizeof(voidptr)))) }
	payload := &AllocatorPayload{ text: 'reachable'.repeat(1024) }
	unsafe { *slot = payload }
	return slot
}

fn test_explicit_allocations_keep_referenced_gc_payloads_alive() {
	mut storage := arena.new()
	defer { storage.free() }
	heap_slot := store_heap_payload(arena.heap_allocator())
	arena_slot := store_heap_payload(storage.allocator())
	assert storage.owns(arena_slot)
	assert !storage.owns(heap_slot)
	gc_collect()
	heap_payload := unsafe { &AllocatorPayload(*heap_slot) }
	arena_payload := unsafe { &AllocatorPayload(*arena_slot) }
	assert heap_payload.text == 'reachable'.repeat(1024)
	assert arena_payload.text == heap_payload.text
	unsafe {
		heap_payload.text.free()
		arena_payload.text.free()
		free(heap_payload)
		free(arena_payload)
		free(heap_slot)
	}
}
