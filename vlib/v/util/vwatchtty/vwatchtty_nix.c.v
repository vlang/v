module vwatchtty

import os

#include <signal.h>
#include <unistd.h>

fn C.getpgrp() int
fn C.kill(pid int, signal int) int
fn C.setpgid(pid int, pgid int) int
fn C.signal(signal int, handler voidptr) voidptr
fn C.tcgetpgrp(fd int) int
fn C.tcsetpgrp(fd int, pgid int) int

// process_group returns the watcher's current process group.
pub fn process_group() int {
	return C.getpgrp()
}

// set_foreground_process_group gives the child process group control of stdin's terminal.
pub fn set_foreground_process_group(pid int, watcher_pgid int) bool {
	if os.is_atty(0) == 0 {
		return false
	}
	// A background watch job must not take the terminal away from the shell.
	if C.tcgetpgrp(0) != watcher_pgid {
		return false
	}
	// The child also calls setpgid before exec. Repeating it in the parent
	// closes the race before the terminal foreground group is changed.
	C.setpgid(pid, pid)
	// Joining the child group makes Ctrl-C reach both the watched command and
	// the watcher, while the manager remains available to reap the worker.
	joined_child_group := C.setpgid(0, pid) == 0
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

// restore_foreground_process_group returns control of stdin's terminal to the watcher.
pub fn restore_foreground_process_group(watcher_pgid int) {
	if os.is_atty(0) == 0 {
		return
	}
	previous_handler := C.signal(C.SIGTTOU, C.SIG_IGN)
	C.tcsetpgrp(0, watcher_pgid)
	C.signal(C.SIGTTOU, previous_handler)
	C.setpgid(0, watcher_pgid)
}
