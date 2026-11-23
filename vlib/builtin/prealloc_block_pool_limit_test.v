// vtest vflags: -prealloc -gc none
module builtin

fn test_prealloc_block_pool_limit_bounds_and_releases_kept_blocks() {
	$if prealloc && !windows {
		$if !prealloc_no_recycle ? {
			unsafe { prealloc_block_pool_set_limit(i64(64) * 1024 * 1024) }
			before := prealloc_block_pool_idle_bytes()
			// A freed scope hands its blocks to the pool.
			scope := unsafe { prealloc_scope_begin() }
			mut total := 0
			for _ in 0 .. 8 {
				chunk := []u8{len: 2 * 1024 * 1024, init: 1}
				total += chunk.len
			}
			unsafe { prealloc_scope_leave(scope) }
			unsafe { prealloc_scope_free_after(scope) }
			kept := prealloc_block_pool_idle_bytes()
			assert total == 16 * 1024 * 1024
			assert kept >= before + i64(total)
			// Lowering the limit returns the excess at once.
			unsafe { prealloc_block_pool_set_limit(i64(4) * 1024 * 1024) }
			assert prealloc_block_pool_idle_bytes() <= i64(4) * 1024 * 1024
			// And a block that does not fit under it is not kept.
			scope2 := unsafe { prealloc_scope_begin() }
			big := []u8{len: 8 * 1024 * 1024, init: 2}
			assert big.len == 8 * 1024 * 1024
			unsafe { prealloc_scope_leave(scope2) }
			unsafe { prealloc_scope_free_after(scope2) }
			assert prealloc_block_pool_idle_bytes() <= i64(4) * 1024 * 1024
			unsafe { prealloc_block_pool_set_limit(0) }
			assert prealloc_block_pool_idle_bytes() == 0
			// The allocator keeps working without kept blocks.
			scope3 := unsafe { prealloc_scope_begin() }
			again := []u8{len: 1024 * 1024, init: 3}
			assert again[again.len - 1] == 3
			unsafe { prealloc_scope_leave(scope3) }
			unsafe { prealloc_scope_free_after(scope3) }
			assert prealloc_block_pool_idle_bytes() == 0
		}
	}
	assert prealloc_block_pool_idle_bytes() >= 0
}
