module ssa

// test_os_process_wait_is_left_out_of_native_builds validates that the process
// methods the native backend cannot compile are excluded from the build rather
// than left in to abort `build_call` when their callee turns out to be missing.
fn test_os_process_wait_is_left_out_of_native_builds() {
	b := Builder{}
	for name in ['Process.wait', 'CommandArgs.close', 'system_args'] {
		assert b.skip_source_fn_in_module(name, 'os')
	}
	// The sibling method stays in: `Process.close` only needs `fd_close`, which
	// the native path declares, so it is buildable and must not be dropped.
	assert !b.skip_source_fn_in_module('Process.close', 'os')
	assert !b.skip_source_fn_in_module('fd_close', 'os')
}

// test_the_os_exclusion_is_not_a_global_method_blacklist validates that the
// exclusion above lives in the module-scoped list, so a method with the same
// name in another module is still compiled. `name` here is the *declaration*
// spelling, `Type.method`, and not the bare `wait`: `skip_source_fn` is shared
// by every module, so moving these entries there would drop unrelated functions.
fn test_the_os_exclusion_is_not_a_global_method_blacklist() {
	b := Builder{}
	assert b.skip_source_fn_in_module('Process.wait', 'os')
	assert !b.skip_source_fn_in_module('Process.wait', 'main')
	assert !b.skip_source_fn_in_module('Process.wait', 'strings')
	assert !b.skip_source_fn('Process.wait')
	assert !b.skip_source_fn('wait')
}

// test_os_process_close_stays_force_used validates that dropping `Process.wait`
// did not take the force-marked pair with it. A body the native path builds
// still calls `Process.close`, and that call has to keep resolving.
fn test_os_process_close_stays_force_used() {
	b := Builder{}
	for name in ['os.Process.close', 'os__Process__close', 'os.fd_close', 'os__fd_close'] {
		assert b.fn_is_used(name)
	}
	// The method that only waits is not the one that closes, and the exclusion for
	// it is a build exclusion, not a used-fn one.
	assert !b.fn_is_used('os.Process.wait')
}
