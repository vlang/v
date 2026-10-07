module bench

import os
import time

const memory_limit_exit_child = 'V3_MEMORY_LIMIT_EXIT_CHILD'
const memory_limit_exit_cleanup_marker = 'memory limit exit ran process cleanup'

fn memory_limit_exit_cleanup() {
	eprintln(memory_limit_exit_cleanup_marker)
}

fn test_memory_limit_exit_skips_concurrent_process_cleanup() {
	mode := os.getenv(memory_limit_exit_child)
	if mode != '' {
		at_exit(memory_limit_exit_cleanup) or { panic(err) }
		if mode == 'stage' {
			mut b := new()
			b.set_memory_limit(1)
			b.start_memory_monitor()
			time.sleep(10 * time.second)
		} else if mode == 'step' {
			mut b := new()
			b.set_memory_limit(1)
			b.step('test')
		} else {
			monitor_memory_limit(1)
		}
		assert false, 'memory limit did not terminate the process'
		return
	}
	for child_mode in ['stage', 'step', 'legacy'] {
		mut child := os.new_process(os.executable())
		mut environment := os.environ()
		environment[memory_limit_exit_child] = child_mode
		child.set_environment(environment)
		child.set_redirect_stdio()
		child.wait()
		error_output := child.stderr_slurp()
		child.close()
		assert child.code == 1, '${child_mode}: ${error_output}'
		assert error_output.contains('compiler memory usage reached'), error_output
		assert !error_output.contains(memory_limit_exit_cleanup_marker), error_output
	}
}
