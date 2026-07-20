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
