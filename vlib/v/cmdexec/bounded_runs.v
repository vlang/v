@[has_globals]
module cmdexec

import os
import sync
import sync.stdatomic
import time

// A bounded run is the leader of a process group of its own, so that its deadline can take
// down everything it started. The other side of that: a signal sent to the group of this
// process (Ctrl-C in a terminal) does not reach it, and when this process dies of the signal,
// the run and its descendants go on, with nobody left to enforce the deadline. So a tool
// that is cancelled ends its bounded runs before it goes.
//
// A run is started by one thread and can be cancelled from any other, at any moment, also
// between the fork that starts it and the moment the thread has noted it anywhere. So the
// cancellation is a flag, and nothing is ended from the signal handler:
// - cancel_bounded_runs sets the flag. It does nothing else, so a signal handler may call it,
//   on whichever thread it runs.
// - The thread that owns a run sees the flag in its read loop and kills the run's group, as
//   it does at the deadline. A run is not started once the flag is set.
// - end_cancelled_bounded_runs, called by a thread of the tool that is not a signal handler,
//   waits until no run is being started and none is left, and then the tool may exit. The
//   count of runs being started is what closes the gap: a thread adds to it before it forks
//   and takes away from it only when its run is in the table.
const max_bounded_runs = 256

__global cmdexec_bounded_runs [256]&os.Process
__global cmdexec_bounded_runs_mutex = sync.new_mutex()
__global cmdexec_bounded_runs_starting u64
__global cmdexec_bounded_runs_cancelled u64

// begin_bounded_start is called by a thread before it starts a bounded run.
fn begin_bounded_start() {
	stdatomic.add_u64(&cmdexec_bounded_runs_starting, 1)
}

// end_bounded_start is called when the run is in the table, or was not started.
fn end_bounded_start() {
	stdatomic.sub_u64(&cmdexec_bounded_runs_starting, 1)
}

// track_bounded_run records a bounded run that has started, and returns its slot (-1 when
// the table is full: the run is then ended by its own thread only).
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

// cancel_bounded_runs asks every bounded run of this process to end, and keeps new ones from
// starting. `reason` is kept for bounded_runs_cancelled (a signal number, for a tool that is
// cancelled by a signal); 0 is taken as 1. It only sets a flag: a signal handler may call it.
pub fn cancel_bounded_runs(reason int) {
	stdatomic.store_u64(&cmdexec_bounded_runs_cancelled, if reason == 0 {
		u64(1)
	} else {
		u64(reason)
	})
}

// bounded_runs_cancelled returns the reason given to cancel_bounded_runs, or 0 when the runs
// have not been cancelled.
pub fn bounded_runs_cancelled() int {
	return int(stdatomic.load_u64(&cmdexec_bounded_runs_cancelled))
}

// end_cancelled_bounded_runs is for the tool that called cancel_bounded_runs, on a thread
// that is not a signal handler: it returns when no bounded run is being started and none is
// left, or after `limit`. Each run is ended by the thread that owns it; the runs in the table
// are also killed from here, so that a thread that is slow to look does not hold the tool up.
// It returns true when nothing was left.
pub fn end_cancelled_bounded_runs(limit time.Duration) bool {
	waited := time.new_stopwatch()
	for {
		kill_bounded_runs()
		if stdatomic.load_u64(&cmdexec_bounded_runs_starting) == 0 && bounded_runs_in_progress() == 0 {
			return true
		}
		if waited.elapsed() >= limit {
			return false
		}
		time.sleep(5 * time.millisecond)
	}
	return false
}

// kill_bounded_runs kills every bounded run in the table together with the processes it
// started, and returns how many there were. The thread that owns a run collects it.
pub fn kill_bounded_runs() int {
	mut killed := 0
	for i in 0 .. max_bounded_runs {
		mut process := cmdexec_bounded_runs[i]
		if isnil(process) {
			continue
		}
		if process.status !in [.running, .stopped] {
			// its owner has collected it already
			continue
		}
		// The group first, then the leader, as for a deadline (see run_in_mode_with_env_timed).
		process.signal_pgkill()
		process.signal_kill()
		// signal_kill() marks it `.aborted`, and its owner's wait() only collects a running one.
		if process.status == .aborted {
			process.status = .running
		}
		killed++
	}
	return killed
}
