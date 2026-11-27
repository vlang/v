@[has_globals]
module cmdexec

import sync.stdatomic
import time

// A bounded run is the leader of a process group of its own, so that its deadline can take
// down everything it started. The other side of that: a signal sent to the group of this
// process (Ctrl-C in a terminal) does not reach it, and when this process dies of the signal,
// the run and its descendants go on, with nobody left to enforce the deadline. So a tool
// that is cancelled ends its bounded runs before it goes.
//
// A run belongs to the thread that started it, from before its fork until it has been waited
// for and closed, and only that thread touches its process. A cancellation can come from any
// other thread at any moment of that, so it is a flag and two words of state, and nothing is
// ended from the signal handler:
// - cancel_bounded_runs sets the flag: a signal handler may call it, on whichever thread it
//   runs.
// - The thread that owns a run sees the flag in its read loop and kills the run's group, as
//   it does at the deadline. A run is not started once the flag is set.
//   A run whose output is at its end is finished by the same thread under one rule: its
//   leader is seen to have ended WITHOUT being reaped (cmdexec_wait.c), and the flag is read
//   after that. A cancellation recorded before the leader ended therefore always ends the
//   group, while the leader's pid is still its own; one recorded after that read finds a
//   run that had ended by itself.
// - The number of runs that have an owner (cmdexec_bounded_runs_active) is counted from
//   before the fork to after the close, so there is no moment at which a started run is in
//   no count. end_cancelled_bounded_runs waits for it to be zero.
// - Whether the tool ends as cancelled or as finished is decided once, by a compare-and-swap
//   on cmdexec_bounded_runs_end: the thread that exits a cancelled tool takes `cancelling`
//   (begin_cancelled_exit), or the thread that finishes the work takes `finished`
//   (finish_bounded_runs). A signal after `finished` ends the process at once with the
//   signal's status, so a signal that was accepted always decides how the tool ends.
const bounded_runs_working = u64(0)
const bounded_runs_cancelling = u64(1)
const bounded_runs_finished = u64(2)

__global cmdexec_bounded_runs_active u64
__global cmdexec_bounded_runs_cancelled u64
__global cmdexec_bounded_runs_end u64

// begin_bounded_run is called by a thread before it starts a bounded run; from here to
// end_bounded_run the run is its own.
fn begin_bounded_run() {
	stdatomic.add_u64(&cmdexec_bounded_runs_active, 1)
}

// end_bounded_run is called when the run has been waited for and closed, or was not started.
fn end_bounded_run() {
	stdatomic.sub_u64(&cmdexec_bounded_runs_active, 1)
}

// bounded_runs_in_progress returns the number of bounded runs that a thread is starting,
// running or collecting.
pub fn bounded_runs_in_progress() int {
	return int(stdatomic.load_u64(&cmdexec_bounded_runs_active))
}

// begin_cancellable_work is called by a tool before it installs a handler that calls
// cancel_bounded_runs: its work is going on, and how it ends is open.
pub fn begin_cancellable_work() {
	stdatomic.store_u64(&cmdexec_bounded_runs_end, bounded_runs_working)
}

// note_cancellation sets the flag, and returns true when the tool's work was finished
// already: the process is then to end at once.
fn note_cancellation(reason int) bool {
	stdatomic.store_u64(&cmdexec_bounded_runs_cancelled, if reason == 0 {
		u64(1)
	} else {
		u64(reason)
	})
	// The flag first, the state second: finish_bounded_runs takes the state first and reads
	// the flag second, so one of the two sees the other.
	return stdatomic.load_u64(&cmdexec_bounded_runs_end) == bounded_runs_finished
}

// cancel_bounded_runs asks every bounded run of this process to end, and keeps new ones from
// starting. `reason` is kept for bounded_runs_cancelled (a signal number, for a tool that is
// cancelled by a signal); 0 is taken as 1. A signal handler may call it. When the tool has
// finished its work (finish_bounded_runs) there is nothing to end and nobody to see the flag:
// the process ends here, with 128 + `reason`, without running exit handlers.
pub fn cancel_bounded_runs(reason int) {
	if note_cancellation(reason) {
		C._exit(i32(128 + bounded_runs_cancelled()))
	}
}

// bounded_runs_cancelled returns the reason given to cancel_bounded_runs, or 0 when the runs
// have not been cancelled.
pub fn bounded_runs_cancelled() int {
	return int(stdatomic.load_u64(&cmdexec_bounded_runs_cancelled))
}

// begin_cancelled_exit is called by the thread that ends a cancelled tool. It returns false
// when the work was finished first: the thread that finished it sees the flag, or the signal
// has ended the process, and this thread has nothing to do.
pub fn begin_cancelled_exit() bool {
	mut expected := bounded_runs_working
	return C.atomic_compare_exchange_strong_u64(voidptr(&cmdexec_bounded_runs_end), &expected,
		bounded_runs_cancelling)
}

// end_cancelled_bounded_runs waits until no bounded run has an owner any more, or `limit` is
// over, and returns true when none was left. Each run is ended by its own thread.
pub fn end_cancelled_bounded_runs(limit time.Duration) bool {
	waited := time.new_stopwatch()
	for {
		if bounded_runs_in_progress() == 0 {
			return true
		}
		if waited.elapsed() >= limit {
			return false
		}
		time.sleep(2 * time.millisecond)
	}
	return false
}

// finish_bounded_runs is called by a tool when its work is over and it is about to report and
// exit. It returns the reason of a cancellation that was accepted before this moment, or 0:
// the tool then exits as cancelled, or goes on to its ordinary end. A signal that arrives
// later ends the process by itself (see cancel_bounded_runs). When another thread is already
// ending the tool as cancelled, this does not return.
pub fn finish_bounded_runs() int {
	mut expected := bounded_runs_working
	if !C.atomic_compare_exchange_strong_u64(voidptr(&cmdexec_bounded_runs_end), &expected,
		bounded_runs_finished) {
		for stdatomic.load_u64(&cmdexec_bounded_runs_end) == bounded_runs_cancelling {
			time.sleep(10 * time.millisecond)
		}
	}
	return bounded_runs_cancelled()
}
