// BEAM backend runtime implementation
// Provides runtime information on the BEAM VM
module runtime

// nr_cpus returns the number of schedulers available on the BEAM VM
// On BEAM, this corresponds to erlang:system_info(schedulers_online)
// By default, BEAM starts one scheduler per CPU core
pub fn nr_cpus() int {
	// BEAM defaults to using all available CPU cores
	// For now, return a reasonable default
	// The actual value would be: erlang:system_info(schedulers_online)
	return 4
}

// total_memory returns total memory available to the BEAM VM
// On BEAM: erlang:memory(total)
pub fn total_memory() !usize {
	// Stub - would call erlang:memory(total)
	return 0
}

// free_memory returns free memory (not directly available on BEAM)
// On BEAM, memory is managed by the VM's garbage collector
pub fn free_memory() !usize {
	return error('free_memory: not directly available on BEAM')
}
