// Regression test for issue #27555: on the optimized `-gc` modes (the default
// `boehm_full_opt`), `main()` calls `GC_set_free_space_divisor(1)` before
// `GC_INIT()`, so the heap is allowed to grow to roughly the live set before
// collecting. That keeps stop-the-world collections rare, which with thread-local
// allocation is what otherwise serializes worker threads on allocation-heavy
// multithreaded HTTP servers. An explicit `GC_FREE_SPACE_DIVISOR` env var is
// applied by `GC_INIT()` afterwards and wins, so the test skips when one is set.
import os

fn C.GC_get_free_space_divisor() usize

fn test_free_space_divisor_is_tuned_for_throughput() {
	$if gcboehm_opt ? {
		if os.getenv('GC_FREE_SPACE_DIVISOR') != '' {
			eprintln('skipping: explicit GC_FREE_SPACE_DIVISOR override is set')
			assert true
			return
		}
		assert C.GC_get_free_space_divisor() == 1
	} $else {
		eprintln('skipping: not an optimized boehm GC build')
		assert true
	}
}
