module driver_signals

import os

// interrupt_process_group sends SIGINT to the process group led by pid.
pub fn interrupt_process_group(pid int) ! {
	$if windows {
		return error('process-group signals are not supported on Windows')
	} $else {
		if C.kill(-pid, int(os.Signal.int)) != 0 {
			return os.last_error()
		}
	}
}
