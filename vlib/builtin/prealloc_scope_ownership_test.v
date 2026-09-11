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
