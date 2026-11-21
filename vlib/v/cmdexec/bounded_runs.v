@[has_globals]
module cmdexec

import os
import sync
import time

// A bounded run is the leader of a process group of its own, so that its deadline can take
// down everything it started. The other side of that: a signal sent to the group of this
// process (Ctrl-C in a terminal) does not reach it, and when this process dies of the signal,
// the run and its descendants go on, with nobody left to enforce the deadline. So the runs in
// progress are kept here, and a tool that is cancelled ends them with kill_bounded_runs
// before it goes.
const max_bounded_runs = 256

__global cmdexec_bounded_runs [256]&os.Process
__global cmdexec_bounded_runs_mutex = sync.new_mutex()

// track_bounded_run records a bounded run that has started, and returns its slot (-1 when
// the table is full: the run is then not ended by kill_bounded_runs).
fn track_bounded_run(process &os.Process) int {
	cmdexec_bounded_runs_mutex.lock()
	defer {
		cmdexec_bounded_runs_mutex.unlock()
	}
	for i in 0 .. max_bounded_runs {
		if isnil(cmdexec_bounded_runs[i]) {
			cmdexec_bounded_runs[i] = unsafe { process }
			return i
		}
	}
	return -1
}

// untrack_bounded_run forgets the run in `slot`: it is over and has been waited for.
fn untrack_bounded_run(slot int) {
	if slot < 0 {
		return
	}
	cmdexec_bounded_runs_mutex.lock()
	cmdexec_bounded_runs[slot] = unsafe { nil }
	cmdexec_bounded_runs_mutex.unlock()
}

// bounded_runs_in_progress returns the number of bounded runs that have started and are not
// over yet.
pub fn bounded_runs_in_progress() int {
	mut n := 0
	for i in 0 .. max_bounded_runs {
		if !isnil(cmdexec_bounded_runs[i]) {
			n++
		}
	}
	return n
}

// kill_bounded_runs kills every bounded run in progress together with the processes it
// started, waits up to two seconds for them to be gone, and returns how many it killed. It is
// for the cancellation of a tool (its handler of SIGINT and SIGTERM): it takes no lock, since
// the thread it interrupts may hold one, and the tool is expected to exit afterwards.
pub fn kill_bounded_runs() int {
	mut killed := []&os.Process{}
	for i in 0 .. max_bounded_runs {
		mut process := cmdexec_bounded_runs[i]
		if isnil(process) {
			continue
		}
		// The group first, then the leader, as for a deadline (see run_in_mode_with_env_timed).
		process.signal_pgkill()
		process.signal_kill()
		// signal_kill() marks it `.aborted`, and is_alive() only asks about a running one.
		if process.status == .aborted {
			process.status = .running
		}
		killed << process
	}
	// is_alive() collects a leader that has exited, so none is left as a zombie of this process.
	waited := time.new_stopwatch()
	for waited.elapsed().milliseconds() < 2000 {
		mut alive := 0
		for mut process in killed {
			if process.is_alive() {
				alive++
			}
		}
		if alive == 0 {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	return killed.len
}
