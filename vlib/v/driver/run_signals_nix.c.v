module driver

import os

#flag @VMODROOT/vlib/v/driver/run_signals_nix.c

fn C.v3_run_signals_begin() i32
fn C.v3_run_signals_set_pid(pid i32) i32
fn C.v3_run_signals_wait(code &i32, signal_number &i32) i32
fn C.v3_run_signals_finish() i32

fn wait_run_process(mut process os.Process) !int {
	if C.v3_run_signals_begin() != 0 {
		return error('could not install run signal handlers')
	}
	process.use_pgroup = true
	process.run()
	if process.pid <= 0 {
		process.status = .aborted
		process.close()
		C.v3_run_signals_finish()
		return error('could not start run process group')
	}
	mut code := i32(0)
	mut signal_number := i32(0)
	if C.v3_run_signals_set_pid(i32(process.pid)) != 0
		|| C.v3_run_signals_wait(&code, &signal_number) != 0 {
		process.signal_kill()
		process.wait()
		C.v3_run_signals_finish()
		return error('could not wait for run process group')
	}
	process.code = int(code)
	process.status = if signal_number != 0 { .aborted } else { .exited }
	if signal_number != 0 {
		process.err = 'Terminated by signal ${signal_number}'
	}
	// The native waiter reaped the child. Finalize it before any later wait/close.
	process.close()
	return C.v3_run_signals_finish()
}
