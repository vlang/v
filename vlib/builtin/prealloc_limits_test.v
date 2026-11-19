// Tests for the opt-in `-d prealloc_limits` cap and fallible allocation.
// Run with:
//   v -prealloc -gc none -d prealloc_limits test vlib/builtin/prealloc_limits_test.v
// The try_* APIs also work under plain -prealloc, where there is no cap.

fn test_prealloc_limit_allows_fits_but_rejects_growth() {
	$if prealloc {
		$if prealloc_limits ? {
			assert prealloc_limit() == 0
			unsafe { prealloc_set_limit(1024) }
			assert prealloc_limit() == 1024

			// Fits in the current block, so the cap is not consulted.
			small := unsafe { prealloc_try_malloc(8) } or {
				assert false
				return
			}
			unsafe { small[0] = 42 }
			assert unsafe { small[0] } == 42

			// Would need a new 20MB block; the cap rejects it without
			// changing allocator state.
			mut big := unsafe { prealloc_try_malloc(20 * 1024 * 1024) }
			assert big == none
			mut zeroed := unsafe { prealloc_try_calloc(20 * 1024 * 1024) }
			assert zeroed == none

			// Removing the cap makes the same request succeed.
			unsafe { prealloc_set_limit(0) }
			assert prealloc_limit() == 0
			big = unsafe { prealloc_try_malloc(20 * 1024 * 1024) }
			assert big != none
			b := big or { panic('unreachable') }
			unsafe { b[0] = 7 }

			// A negative limit is clamped to zero (unlimited).
			unsafe { prealloc_set_limit(-1) }
			assert prealloc_limit() == 0
			zeroed = unsafe { prealloc_try_calloc(64) }
			assert zeroed != none
			z := zeroed or { panic('unreachable') }
			unsafe { assert z[0] == 0 }
		}
	}
}

fn test_prealloc_try_zero_is_none() {
	$if prealloc {
		assert unsafe { prealloc_try_malloc(0) } == none
		assert unsafe { prealloc_try_calloc(0) } == none
	} $else {
		assert unsafe { prealloc_try_malloc(8) } == none
	}
}
