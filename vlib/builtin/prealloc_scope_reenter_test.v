// These tests only exercise the arena with `-prealloc`, e.g.
// `v -prealloc test vlib/builtin/prealloc_scope_reenter_test.v`.
// Scope blocks are 256 KB, so every 200 KB allocation below starts a new block.
const reenter_alloc_size = 200 * 1024

const reenter_keep_all = isize(64 * 1024 * 1024)

fn test_prealloc_scope_reenter_reuses_rewound_blocks() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		first := []u8{len: reenter_alloc_size, init: 7}
		second := []u8{len: reenter_alloc_size, init: 9}
		first_ptr := first.data
		second_ptr := second.data
		unsafe { prealloc_scope_leave(scope) }
		assert unsafe { prealloc_scope_reenter(scope, reenter_keep_all) }
		again_first := []u8{len: reenter_alloc_size}
		again_second := []u8{len: reenter_alloc_size}
		assert again_first.data == first_ptr
		assert again_second.data == second_ptr
		// Reused memory is zeroed like fresh memory.
		assert again_first[0] == 0 && again_second[reenter_alloc_size - 1] == 0
		assert unsafe { prealloc_scope_owns(scope, again_second.data) }
		unsafe { prealloc_scope_leave(scope) }
		after := []u8{len: 32}
		assert !unsafe { prealloc_scope_owns(scope, after.data) }
		unsafe { prealloc_scope_free_after(scope) }
	}
}

fn test_prealloc_scope_reenter_refuses_a_current_scope() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		data := []u8{len: 64, init: 3}
		assert !unsafe { prealloc_scope_reenter(scope, reenter_keep_all) }
		// The refused call left the scope and its data untouched.
		assert data[63] == 3
		assert unsafe { prealloc_scope_owns(scope, data.data) }
		unsafe { prealloc_scope_leave(scope) }
		unsafe { prealloc_scope_free_after(scope) }
	}
}

fn test_prealloc_scope_reenter_frees_blocks_beyond_the_budget() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		first := []u8{len: reenter_alloc_size}
		second := []u8{len: reenter_alloc_size}
		third := []u8{len: reenter_alloc_size}
		first_ptr := first.data
		second_ptr := second.data
		third_ptr := third.data
		unsafe { prealloc_scope_leave(scope) }
		// The first block is always kept; the others exceed a 1 byte budget.
		assert unsafe { prealloc_scope_reenter(scope, 1) }
		assert unsafe { prealloc_scope_owns(scope, first_ptr) }
		assert !unsafe { prealloc_scope_owns(scope, second_ptr) }
		assert !unsafe { prealloc_scope_owns(scope, third_ptr) }
		again := []u8{len: reenter_alloc_size}
		assert again.data == first_ptr
		unsafe { prealloc_scope_leave(scope) }
		unsafe { prealloc_scope_free_after(scope) }
	}
}

fn test_prealloc_nested_scope_keeps_the_rewound_blocks_of_its_parent() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		first := []u8{len: reenter_alloc_size}
		second := []u8{len: reenter_alloc_size}
		first_ptr := first.data
		second_ptr := second.data
		unsafe { prealloc_scope_leave(scope) }
		assert unsafe { prealloc_scope_reenter(scope, reenter_keep_all) }
		// A nested scope is linked after the rewound first block. Unlinking it
		// must reattach the rewound second block instead of dropping it.
		nested := unsafe { prealloc_scope_begin() }
		nested_data := []u8{len: 64}
		assert unsafe { prealloc_scope_owns(nested, nested_data.data) }
		unsafe { prealloc_scope_leave(nested) }
		unsafe { prealloc_scope_free_after(nested) }
		again_first := []u8{len: reenter_alloc_size}
		again_second := []u8{len: reenter_alloc_size}
		assert again_first.data == first_ptr
		assert again_second.data == second_ptr
		unsafe { prealloc_scope_leave(scope) }
		unsafe { prealloc_scope_free_after(scope) }
	}
}
