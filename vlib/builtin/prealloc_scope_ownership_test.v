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
