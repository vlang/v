module arm64

// test_getrusage_is_always_linked_from_the_system_library validates this v3 regression case.
fn test_getrusage_is_always_linked_from_the_system_library() {
	assert '_getrusage' in force_external_syms
	assert '_mach_task_self' in force_external_syms
	assert '_mach_task_self_' in force_external_syms
}
