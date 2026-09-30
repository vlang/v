// vtest vflags: -d v3_no_parallel
module markused

import os

// In a `v3_no_parallel` build the runtime-helper scan must stay on the main
// thread: its worker thread queries a checker fork, even when the driver has a
// worker pool running.
fn test_no_parallel_build_keeps_runtime_helper_scan_on_the_main_thread() {
	old := os.getenv_opt('V3_NO_PAR_MU_SEEDS')
	os.unsetenv('V3_NO_PAR_MU_SEEDS')
	defer {
		if value := old {
			os.setenv('V3_NO_PAR_MU_SEEDS', value, true)
		}
	}
	assert !par_markused_seeds_enabled()
}
