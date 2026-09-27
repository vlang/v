// vtest vflags: -d v3_no_parallel
module driver

// A `v3_no_parallel` build resolves native inputs on the main thread instead of
// overlapping them with the checker's declaration pass.
fn test_no_parallel_build_never_overlaps_native_inputs() {
	assert !should_overlap_v3_native_inputs('c', false, false, false, false, true)
	assert !should_overlap_v3_native_inputs('c', false, false, true, true, false)
	assert !should_overlap_v3_native_inputs('c', false, false, true, true, true)
}
