// Tests for the opt-in live prealloc statistics and chunk iterator.
// Run with:
//   v -prealloc -gc none -d prealloc_stats test vlib/builtin/prealloc_stats_test.v
// The iterator and chunk-capacity queries work under -prealloc without any
// extra define; the counters require -d prealloc_stats.

fn test_prealloc_live_stats_and_chunk_iter() {
	$if prealloc {
		$if prealloc_stats ? {
			before := prealloc_stats_snapshot()
			assert before.enabled

			scope := unsafe { prealloc_scope_begin() }
			mut buffers := []voidptr{cap: 12}
			for _ in 0 .. 12 {
				buffers << unsafe { malloc(70 * 1024) }
			}

			during := prealloc_stats_snapshot()
			assert during.allocation_count > before.allocation_count
			assert during.allocated_bytes >= before.allocated_bytes + u64(12 * 70 * 1024)
			assert during.mapped_bytes >= u64(256 * 1024)
			assert during.block_count > before.block_count
			assert during.used_bytes >= u64(12 * 70 * 1024)

			mut iter := unsafe { prealloc_scope_chunk_iter(scope) }
			mut chunks := 0
			mut capacity_sum := u64(0)
			for {
				chunk := unsafe { iter.next() } or { break }
				chunks++
				capacity_sum += chunk.capacity
				assert chunk.capacity > 0
				assert chunk.used <= chunk.capacity
			}
			assert chunks >= 1
			assert capacity_sum == u64(unsafe { prealloc_scope_allocated_size(scope) })

			capacity_before := prealloc_current_chunk_capacity()
			assert capacity_before > 0
			unsafe { malloc(1024) }
			assert prealloc_current_chunk_capacity() <= capacity_before

			// Rewinding a scope returns its bumped bytes to the live counter.
			used_before := prealloc_stats_snapshot().used_bytes
			unsafe { prealloc_scope_leave(scope) }
			assert unsafe { prealloc_scope_reenter(scope, 64 * 1024 * 1024) }
			used_after := prealloc_stats_snapshot().used_bytes
			assert used_after < used_before

			unsafe {
				prealloc_scope_leave(scope)
				prealloc_scope_end(scope)
			}
			after := prealloc_stats_snapshot()
			assert after.block_count >= before.block_count
		} $else {
			disabled := prealloc_stats_snapshot()
			assert !disabled.enabled
			assert disabled.mapped_bytes == 0
			assert disabled.used_bytes == 0
			assert disabled.block_count == 0
		}
	}
}

fn test_prealloc_chunk_iter_without_stats_define() {
	$if prealloc {
		assert prealloc_current_chunk_capacity() > 0
		scope := unsafe { prealloc_scope_begin() }
		mut iter := unsafe { prealloc_scope_chunk_iter(scope) }
		assert unsafe { iter.next() } != none
		unsafe { prealloc_scope_end(scope) }
	}
}
