// vtest vflags: -d v3_no_parallel
module driver

// A `v3_no_parallel` build keeps transform, and the markused and transform
// preparation threads that read the checker beside the main thread, off.
fn test_no_parallel_build_disallows_parallel_transform() {
	assert !v3_parallel_transform_allowed(true, false)
	assert !v3_parallel_transform_allowed(true, true)
}
