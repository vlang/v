module bench

import os

const memory_limit_exit_child = 'V3_MEMORY_LIMIT_EXIT_CHILD'
const memory_limit_exit_cleanup_marker = 'memory limit exit ran process cleanup'

fn memory_limit_exit_cleanup() {
	eprintln(memory_limit_exit_cleanup_marker)
}

fn test_memory_limit_exit_skips_concurrent_process_cleanup() {
	if os.getenv(memory_limit_exit_child) == '1' {
		at_exit(memory_limit_exit_cleanup) or { panic(err) }
		monitor_memory_limit(1)
		assert false
		return
	}
	mut child := os.new_process(os.executable())
	mut environment := os.environ()
	environment[memory_limit_exit_child] = '1'
	child.set_environment(environment)
	child.set_redirect_stdio()
	child.wait()
	error_output := child.stderr_slurp()
	child.close()
	assert child.code == 1, error_output
	assert error_output.contains('compiler memory usage reached'), error_output
	assert !error_output.contains(memory_limit_exit_cleanup_marker), error_output
	assert !error_output.contains('segmentation fault'), error_output
}
