fn test_prealloc_scope_owns_multiple_blocks() {
	$if prealloc {
		before := []u8{len: 32}
		scope := unsafe { prealloc_scope_begin() }
		first := []u8{len: 512 * 1024}
		second := []u8{len: 512 * 1024}
		assert unsafe { prealloc_scope_owns(scope, first.data) }
		assert unsafe { prealloc_scope_owns(scope, second.data) }
		assert !unsafe { prealloc_scope_owns(scope, before.data) }
		unsafe { prealloc_scope_leave(scope) }
		after := []u8{len: 32}
		assert !unsafe { prealloc_scope_owns(scope, after.data) }
		unsafe { prealloc_scope_free_after(scope) }
	}
}

fn test_prealloc_scope_suspend_allocates_in_parent() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		scoped := 'scoped allocation'.clone()
		state := unsafe { prealloc_scope_suspend(scope) }
		parent := 'parent allocation'.clone()
		unsafe { prealloc_scope_resume(scope, state) }
		resumed := 'resumed scoped allocation'.clone()
		assert unsafe { prealloc_scope_owns(scope, scoped.str) }
		assert !unsafe { prealloc_scope_owns(scope, parent.str) }
		assert unsafe { prealloc_scope_owns(scope, resumed.str) }
		unsafe { prealloc_scope_leave(scope) }
		assert parent == 'parent allocation'
		unsafe { prealloc_scope_free_after(scope) }
		assert parent == 'parent allocation'
	}
}

fn recycle_scopes_on_worker(worker_id int) int {
	$if prealloc {
		for iteration in 0 .. 256 {
			scope := unsafe { prealloc_scope_begin() }
			mut data := []u8{len: 384 * 1024}
			data[0] = u8(worker_id)
			data[data.len - 1] = u8(iteration)
			checksum := int(data[0]) + int(data[data.len - 1])
			unsafe { prealloc_scope_leave(scope) }
			unsafe { prealloc_scope_free_after(scope) }
			if unsafe { g_memory_block == nil } {
				return -1
			}
			if checksum != worker_id + int(u8(iteration)) {
				return -1
			}
		}
		unsafe { prealloc_thread_cleanup() }
		if unsafe { g_memory_block != nil } {
			return -1
		}
	}
	return worker_id
}

fn test_prealloc_scope_recycling_is_thread_local() {
	$if prealloc {
		mut threads := []thread int{}
		for worker_id in 1 .. 9 {
			threads << spawn recycle_scopes_on_worker(worker_id)
		}
		for worker_id, handle in threads {
			assert handle.wait() == worker_id + 1
		}
	}
}

fn test_prealloc_refills_preserve_aligned_allocations() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		mut allocations := []&u8{cap: 64}
		mut sizes := []int{cap: 64}
		for i in 0 .. 64 {
			size := 16_385 + i * 19
			alignment := isize(16 << (i % 5))
			source := []u8{len: size, init: u8(i)}
			ptr := unsafe { &u8(memdup_align(source.data, isize(size), alignment)) }
			assert usize(ptr) % usize(alignment) == 0
			assert unsafe { prealloc_scope_owns(scope, ptr) }
			allocations << ptr
			sizes << size
		}
		// Later refills must preserve every earlier block and its contents.
		for i, ptr in allocations {
			assert unsafe { ptr[0] } == u8(i)
			assert unsafe { ptr[sizes[i] - 1] } == u8(i)
		}
		unsafe { prealloc_scope_end(scope) }
	}
}

fn test_prealloc_refills_preserve_allocation_statistics() {
	$if prealloc_stats ? {
		$if prealloc {
			scope := unsafe { prealloc_scope_begin() }
			source := [33]u8{}
			before := prealloc_stats_snapshot()
			small := unsafe { malloc(1) }
			large := unsafe { malloc(2 * 1024 * 1024) }
			aligned := unsafe { memdup_align(&source[0], 33, 256) }
			after := prealloc_stats_snapshot()
			assert after.allocation_count - before.allocation_count == 3
			assert after.allocated_bytes - before.allocated_bytes == 2 * 1024 * 1024 + 34
			assert unsafe { prealloc_scope_owns(scope, small) }
			assert unsafe { prealloc_scope_owns(scope, large) }
			assert unsafe { prealloc_scope_owns(scope, aligned) }
			unsafe { prealloc_scope_end(scope) }
		}
	}
}
