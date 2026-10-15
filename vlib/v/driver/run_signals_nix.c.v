module driver

import os

#flag @VMODROOT/vlib/v/driver/run_signals_nix.c

fn C.v3_run_signals_begin() i32
fn C.v3_run_signals_set_pid(pid i32)
fn C.v3_run_signals_finish() i32

fn wait_run_process(mut process os.Process) !int {
	if C.v3_run_signals_begin() != 0 {
		return error('could not install termination signal handlers')
	}
	process.run()
	C.v3_run_signals_set_pid(i32(process.pid))
	process.wait()
	return C.v3_run_signals_finish()
}
