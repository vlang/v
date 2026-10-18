module ssa

fn test_native_process_methods_are_not_excluded_from_codegen() {
	b := Builder{}
	for name in ['Process.wait', 'CommandArgs.close', 'system_args'] {
		// These methods can be used by native programs. Excluding their source
		// unconditionally leaves a used call without a function definition.
		assert !b.skip_source_fn_in_module(name, 'os')
	}
	assert !b.skip_source_fn_in_module('Process.close', 'os')
	assert !b.skip_source_fn_in_module('fd_close', 'os')
}

fn test_native_process_method_names_are_not_a_global_blacklist() {
	b := Builder{}
	assert !b.skip_source_fn_in_module('Process.wait', 'main')
	assert !b.skip_source_fn_in_module('Process.wait', 'strings')
	assert !b.skip_source_fn('Process.wait')
	assert !b.skip_source_fn('wait')
}

fn test_os_process_close_stays_force_used() {
	b := Builder{}
	for name in ['os.Process.close', 'os__Process__close', 'os.fd_close', 'os__fd_close'] {
		assert b.fn_is_used(name)
	}
	// Process.wait follows normal reachability instead of being force-rooted.
	assert !b.fn_is_used('os.Process.wait')
}
