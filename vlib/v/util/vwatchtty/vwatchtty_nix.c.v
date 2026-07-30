module vwatchtty

import os

#include <signal.h>
#include <unistd.h>

fn C.getpgrp() int
fn C.getpgid(pid int) int
fn C.getpid() int
fn C.kill(pid int, signal int) int
fn C.setpgid(pid int, pgid int) int
fn C.signal(signal int, handler voidptr) voidptr
fn C.tcgetpgrp(fd int) int
fn C.tcsetpgrp(fd int, pgid int) int

// process_group returns the watcher's current process group.
pub fn process_group() int {
	return C.getpgrp()
}

// continue_signal returns the platform's SIGCONT value.
pub fn continue_signal() os.Signal {
	// The C constant is the authoritative value for the active Unix platform.
	return unsafe { os.Signal(C.SIGCONT) }
}

// terminal_stop_signals returns the platform's terminal stop signal values.
pub fn terminal_stop_signals() []os.Signal {
	// The C constants are the authoritative values for the active Unix platform.
	return [unsafe { os.Signal(C.SIGTSTP) }, unsafe { os.Signal(C.SIGTTIN) }, unsafe { os.Signal(C.SIGTTOU) }]
}

// suspend_manager_process_group stops the manager's original process group,
// then stops the foreground worker that received SIGTSTP.
pub fn suspend_manager_process_group(manager_pgid int) {
	if C.getpgrp() != manager_pgid {
		C.kill(-manager_pgid, C.SIGTSTP)
	}
	C.kill(C.getpid(), C.SIGSTOP)
}

// continue_process_group_of resumes the worker and watched child after the
// shell continues the manager job.
pub fn continue_process_group_of(pid int) {
	pgid := C.getpgid(pid)
	if pgid <= 0 {
		return
	}
	if os.is_atty(0) != 0 && C.tcgetpgrp(0) == C.getpgrp() {
		previous_handler := C.signal(C.SIGTTOU, C.SIG_IGN)
		C.tcsetpgrp(0, pgid)
		C.signal(C.SIGTTOU, previous_handler)
	}
	C.kill(-pgid, C.SIGCONT)
}

// set_foreground_process_group gives the child process group control of stdin's terminal.
pub fn set_foreground_process_group(pid int, watcher_pgid int) bool {
	if os.is_atty(0) == 0 {
		return false
	}
	// The child also calls setpgid before exec. Repeating it in the parent
	// closes the race before the terminal foreground group is changed.
	C.setpgid(pid, pid)
	// Joining the child group makes terminal signals reach both the watched
	// command and the worker, while the manager remains available to reap it.
	joined_child_group := C.setpgid(0, pid) == 0
	// A background watch job must not take the terminal away from the shell.
	// It remains in the child group so that a child stop also stops the worker.
	if C.tcgetpgrp(0) != watcher_pgid {
		return false
	}
	previous_handler := C.signal(C.SIGTTOU, C.SIG_IGN)
	result := C.tcsetpgrp(0, pid)
	C.signal(C.SIGTTOU, previous_handler)
	if result == 0 {
		// The child may already have stopped with SIGTTIN before the parent
		// could foreground it.
		C.kill(-pid, C.SIGCONT)
		return true
	}
	if joined_child_group {
		C.setpgid(0, watcher_pgid)
	}
	return false
}

// restore_foreground_process_group returns terminal control to the watcher only
// when the current worker process group still owns it.
pub fn restore_foreground_process_group(watcher_pgid int) {
	// A worker resumed with `bg` no longer owns the terminal. In that case,
	// leave it with the shell and only rejoin the watcher process group.
	if os.is_atty(0) != 0 && C.tcgetpgrp(0) == C.getpgrp() {
		previous_handler := C.signal(C.SIGTTOU, C.SIG_IGN)
		C.tcsetpgrp(0, watcher_pgid)
		C.signal(C.SIGTTOU, previous_handler)
	}
	C.setpgid(0, watcher_pgid)
}
