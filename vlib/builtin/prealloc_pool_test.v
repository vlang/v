// Verifies the size-class recycle pool used on non-Windows targets.
// Run with: v -prealloc -gc none test vlib/builtin/prealloc_pool_test.v
module builtin

fn test_prealloc_block_pool_size_classes() {
	$if prealloc {
		$if !windows {
			sizes := [64 * 1024, 128 * 1024, 256 * 1024, 512 * 1024]
			mut ptrs := []voidptr{cap: sizes.len}
			for sz in sizes {
				p := unsafe { C.malloc(sz) }
				assert p != unsafe { nil }
				ptrs << p
				assert unsafe { prealloc_block_pool_put(p, sz) }
			}
			// A 200KB request should get the 256KB class, not the largest block.
			p, sz := unsafe { prealloc_block_pool_take(200 * 1024) }
			assert p != unsafe { nil }
			assert sz == 256 * 1024
			unsafe { C.free(p) }
			// Drain the rest so the shared pool is empty again.
			for {
				q, qsz := unsafe { prealloc_block_pool_take(1) }
				if q == unsafe { nil } {
					break
				}
				assert qsz > 0
				unsafe { C.free(q) }
			}
		}
	}
}
