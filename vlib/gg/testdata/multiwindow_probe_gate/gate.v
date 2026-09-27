module multiwindow_probe_gate

import os
import time

pub const environment_name = 'V_MULTIWINDOW_PROBE_WATCHDOG_GATE'

const poll_interval = 5 * time.millisecond

// await_parent_release prevents probe work from starting before the parent has
// attached the process group or Windows Job Object.
pub fn await_parent_release(timeout time.Duration) ! {
	gate_path := os.getenv(environment_name)
	if gate_path == '' {
		return
	}
	if timeout <= 0 {
		return error('multi-window probe gate: timeout must be positive')
	}
	deadline := time.now().add(timeout)
	for !os.exists(gate_path) {
		if time.now() >= deadline {
			return error('multi-window probe gate: parent did not release `${gate_path}`')
		}
		time.sleep(poll_interval)
	}
}
