// Verifies that -d track_heap works with -prealloc at block granularity.
// Run with:
//   v -prealloc -gc none -d track_heap -d track_heap_test_stub test vlib/builtin/prealloc_track_heap_test.v
module builtin

fn C.v_prealloc_test_heap_counts(allocs &int, frees &int) int

fn test_prealloc_track_heap_reports_arena_blocks() {
	$if track_heap ? {
		mut allocs0 := 0
		mut frees0 := 0
		C.v_prealloc_test_heap_counts(&allocs0, &frees0)
		assert allocs0 > 0

		scope := unsafe { prealloc_scope_begin() }
		mut buffers := []voidptr{cap: 8}
		for _ in 0 .. 8 {
			buffers << unsafe { malloc(200 * 1024) }
		}

		mut allocs1 := 0
		mut frees1 := 0
		C.v_prealloc_test_heap_counts(&allocs1, &frees1)
		assert allocs1 > allocs0

		unsafe { prealloc_scope_end(scope) }
		mut allocs2 := 0
		mut frees2 := 0
		C.v_prealloc_test_heap_counts(&allocs2, &frees2)
		assert frees2 > frees1
	}
}
