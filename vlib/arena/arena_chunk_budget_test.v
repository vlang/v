import arena

struct Attempt {
mut:
	denied bool
	ptr    voidptr
}

fn allocate(a &arena.Arena, bytes int, mut result Attempt) {
	defer {
		if message := recover() {
			if (arena.allocation_failure() or { panic(message) }) != .chunk_limit { panic(message) }
			result.denied = true
		}
	}
	a.push()
	defer { a.pop() }
	result.ptr = unsafe { vcalloc(bytes) }
}

fn try_allocate(a &arena.Arena, bytes int) bool {
	mut result := Attempt{}
	allocate(a, bytes, mut result)
	return !result.denied && !isnil(result.ptr)
}

fn inherited(a &arena.Arena, limits ?arena.ChunkLimits) arena.Arena {
	a.push()
	defer { a.pop() }
	return arena.new(chunk_size: 128, limits: limits)
}

fn test_zero_budget_denies_every_first_chunk_without_selecting_another_allocator() {
	mut a := arena.new(limits: arena.ChunkLimits{ max_live_bytes: 0 })
	defer { a.free() }
	assert !try_allocate(a, 1)
	assert !arena.active()
	assert a.capacity() == 0
	assert (a.chunk_usage() or { panic('missing budget') }) == arena.ChunkUsage{}
}

fn test_budget_clamps_growth_and_keeps_live_and_cumulative_counts_distinct() {
	mut root := arena.new(limits: arena.ChunkLimits{ max_live_bytes: 512, max_total_bytes: 768 })
	defer { root.free() }
	mut first := inherited(&root, none)
	assert try_allocate(&first, 8)
	initial := root.chunk_usage() or { panic('missing parent budget') }
	assert initial.live_bytes == initial.total_bytes
	assert initial.live_bytes > 128 && initial.live_bytes <= 512
	assert (first.chunk_usage() or { panic('missing budget') }) == initial
	assert !try_allocate(&root, 512)
	assert (root.chunk_usage() or { panic('missing budget') }) == initial
	first.reset()
	assert (root.chunk_usage() or { panic('missing budget') }) == initial
	first.free()
	assert (root.chunk_usage() or { panic('missing budget') }) == arena.ChunkUsage{
		peak_bytes:  initial.live_bytes
		total_bytes: initial.live_bytes
	}
	mut remaining := inherited(&root, none)
	defer { remaining.free() }
	assert try_allocate(&root, 8)
	assert (root.chunk_usage() or { panic('missing budget') }).live_bytes == 512
	assert !try_allocate(&remaining, 1)
	root.reset()
	assert !try_allocate(&remaining, 1)
}

fn test_child_inherits_parent_limits_and_keeps_the_budget_alive_after_parent_free() {
	mut root := arena.new(limits: arena.ChunkLimits{ max_total_bytes: 220 })
	mut child := inherited(&root, arena.ChunkLimits{ max_live_bytes: 200 })
	mut leaf := inherited(&child, none)
	root.free()
	child.free()
	defer { leaf.free() }
	assert try_allocate(&leaf, 15)
	assert !try_allocate(&leaf, 64 * 1024)
	usage := leaf.chunk_usage() or { panic('missing child budget') }
	assert usage.total_bytes <= 200
}

fn test_nested_budget_rejection_does_not_consume_parent_capacity() {
	mut root := arena.new(limits: arena.ChunkLimits{ max_total_bytes: 384 })
	defer { root.free() }
	mut child := inherited(&root, arena.ChunkLimits{ max_total_bytes: 64 })
	defer { child.free() }
	assert !try_allocate(&child, 512)
	assert (root.chunk_usage() or { panic('missing budget') }) == arena.ChunkUsage{}
	assert try_allocate(&root, 8)
	assert (root.chunk_usage() or { panic('missing budget') }).total_bytes == 384
	assert !try_allocate(&child, 1)
}

fn test_cumulative_limit_survives_release_even_when_no_chunks_are_live() {
	mut root := arena.new(limits: arena.ChunkLimits{ max_total_bytes: 384 })
	defer { root.free() }
	mut child := inherited(&root, none)
	assert try_allocate(&child, 320)
	child.free()
	before := root.chunk_usage() or { panic('missing budget') }
	assert before.live_bytes == 0 && before.total_bytes > 320
	assert !try_allocate(&root, 64)
	assert (root.chunk_usage() or { panic('missing budget') }) == before
}

fn competing_child(a &arena.Arena, start chan bool) bool {
	_ := <-start
	return try_allocate(a, 48)
}

fn test_concurrent_children_compete_for_the_same_last_chunk() {
	mut root := arena.new(limits: arena.ChunkLimits{ max_live_bytes: 160 })
	defer { root.free() }
	mut left := inherited(&root, none)
	mut right := inherited(&root, none)
	defer { left.free() }
	defer { right.free() }
	start := chan bool{cap: 2}
	first := spawn competing_child(&left, start)
	second := spawn competing_child(&right, start)
	start <- true
	start <- true
	did_first := first.wait()
	did_second := second.wait()
	assert did_first != did_second
	usage := root.chunk_usage() or { panic('missing budget') }
	assert usage.peak_bytes == usage.live_bytes && usage.total_bytes == usage.live_bytes
	assert usage.live_bytes <= 160
}

fn recover_with_arena_still_selected(bytes int, mut result Attempt) {
	defer {
		if message := recover() {
			result.denied = (arena.allocation_failure() or { panic(message) }) == .chunk_limit
		}
	}
	result.ptr = unsafe { vcalloc(bytes) }
}

fn test_quota_failure_can_be_recovered_while_the_same_exhausted_arena_is_current() {
	mut a := arena.new(limits: arena.ChunkLimits{ max_total_bytes: 0 })
	defer { a.free() }
	mut result := Attempt{}
	a.push()
	recover_with_arena_still_selected(1, mut result)
	a.pop()
	assert result.denied
	assert arena.allocation_failure() == none
}

fn ordinary_allocator_message(mut result Attempt) {
	defer {
		if _ := recover() { result.denied = arena.allocation_failure() == none }
	}
	panic('arena: chunk reservation limit exceeded')
}

fn test_a_host_panic_with_the_same_message_is_distinguishable() {
	mut result := Attempt{}
	ordinary_allocator_message(mut result)
	assert result.denied
}
