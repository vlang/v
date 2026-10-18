module builtin

fn test_prealloc_string_buffers_pack_bytes_and_preserve_general_alignment() {
	$if prealloc && !builtin_arena ?&& !trace_malloc ? {
		scope := unsafe { prealloc_scope_begin() }
		first := 'ab'.clone()
		second := 'cdef'.clone()
		third := 'gh'.clone()
		assert usize(second.str) == usize(first.str) + usize(first.len) + 1
		assert usize(third.str) == usize(second.str) + usize(second.len) + 1
		assert first == 'ab' && second == 'cdef' && third == 'gh'
		assert unsafe { first.str[first.len] } == 0
		assert unsafe { second.str[second.len] } == 0
		general := unsafe { malloc(17) }
		assert usize(general) % usize(prealloc_default_align) == 0
		assert unsafe { prealloc_scope_owns(scope, first.str) }
		assert unsafe { prealloc_scope_owns(scope, general) }
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
		}
	}
}

fn test_prealloc_string_buffers_grow_and_reenter_a_scope() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		for pass in 0 .. 2 {
			if pass > 0 {
				assert unsafe { prealloc_scope_reenter(scope, 0) }
			}
			prefix := 'prefix'.clone()
			large := 'x'.repeat(int(prealloc_scope_block_size) * 2 + 17)
			joined := prefix + large
			assert joined.len == prefix.len + large.len
			assert joined.starts_with('prefix')
			assert joined[joined.len - 1] == `x`
			assert unsafe { joined.str[joined.len] } == 0
			assert unsafe { prealloc_scope_owns(scope, joined.str) }
			general := unsafe { malloc(33) }
			assert usize(general) % usize(prealloc_default_align) == 0
			unsafe { prealloc_scope_leave(scope) }
		}
		unsafe { prealloc_scope_free_after(scope) }
	}
}
