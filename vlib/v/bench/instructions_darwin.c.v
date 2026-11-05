module bench

import os

const rusage_info_v4 = 4

// current_instruction_count returns the retired CPU instructions of the whole
// process (all threads) since it started.
fn current_instruction_count() ?u64 {
	$if macos || ios {
		mut usage := V3RusageInfo{}
		if C.proc_pid_rusage(os.getpid(), rusage_info_v4, &usage) == 0 && usage.ri_instructions > 0 {
			return usage.ri_instructions
		}
	}
	return none
}
