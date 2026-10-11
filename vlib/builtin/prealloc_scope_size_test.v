module builtin

fn test_prealloc_scope_allocated_size_tracks_multiple_blocks() {
	assert unsafe { prealloc_scope_allocated_size(nil) } == 0
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		initial := unsafe { prealloc_scope_allocated_size(scope) }
		first := []u8{len: 512 * 1024}
		first_size := unsafe { prealloc_scope_allocated_size(scope) }
		second := []u8{len: 512 * 1024}
		second_size := unsafe { prealloc_scope_allocated_size(scope) }
		assert initial >= usize(prealloc_scope_block_size)
		assert first_size > initial
		assert second_size > first_size
		assert unsafe { prealloc_scope_owns(scope, first.data) }
		assert unsafe { prealloc_scope_owns(scope, second.data) }
		unsafe { prealloc_scope_leave(scope) }
		assert unsafe { prealloc_scope_allocated_size(scope) } == second_size
		unsafe { prealloc_scope_free_after(scope) }
	}
}

fn test_prealloc_scope_allocated_size_saturates_overflow() {
	mut ranges := [VPreallocRange{ start: 1, stop: ~usize(0) }, VPreallocRange{ start: 5, stop: 6 }]
	scope := VPreallocScope{
		ranges:     unsafe { &ranges[0] }
		ranges_len: ranges.len
	}
	assert unsafe { prealloc_scope_allocated_size(&scope) } == ~usize(0)
	ranges[1].stop = 7
	assert unsafe { prealloc_scope_allocated_size(&scope) } == ~usize(0)
}
