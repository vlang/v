@[has_globals]
module driver

import os

// A `-cg` build sets the C sources of the executable in place aside while it runs (see
// v3_set_aside_c_debug_dir), and a build that fails has to give them back. A build has many
// ways to end: `run` alone has dozens of `exit()` calls after that point, and the functions it
// calls have their own. So giving the sources back does not depend on every one of them
// calling cleanup_c_build_dir: an exit hook does it for whichever exit is taken. The path is
// kept in fixed storage, out of the stage arenas that a build frees while it runs.
const max_c_debug_dir_path = 4096

__global v3_c_debug_dir_watched [4096]u8
__global v3_c_debug_dir_watched_len int
__global v3_c_debug_dir_watched_pid int
__global v3_c_debug_dir_hook_registered bool
__global v3_c_debug_dir_untouched [4096]u8
__global v3_c_debug_dir_untouched_len int

// v3_keep_c_debug_dir_untouched records `cc_dir` as a directory this process must not change:
// a `-cg` build was refused there, and what it holds belongs to the executable in place. A
// path too long for the storage cannot be recorded; it is not watched either, and the build
// ends where it was refused.
fn v3_keep_c_debug_dir_untouched(cc_dir string) {
	v3_c_debug_dir_untouched_len = 0
	if cc_dir.len == 0 || cc_dir.len >= max_c_debug_dir_path {
		return
	}
	for i in 0 .. cc_dir.len {
		v3_c_debug_dir_untouched[i] = cc_dir[i]
	}
	v3_c_debug_dir_untouched_len = cc_dir.len
}

// v3_forget_untouched_c_debug_dir is called when a `-cg` build is tried again.
fn v3_forget_untouched_c_debug_dir() {
	v3_c_debug_dir_untouched_len = 0
}

// v3_untouched_c_debug_dir returns the directory a refused build left alone, or '' for none.
fn v3_untouched_c_debug_dir() string {
	if v3_c_debug_dir_untouched_len == 0 {
		return ''
	}
	mut path := []u8{cap: v3_c_debug_dir_untouched_len}
	for i in 0 .. v3_c_debug_dir_untouched_len {
		path << v3_c_debug_dir_untouched[i]
	}
	return path.bytestr()
}

// v3_watch_c_debug_dir records `cc_dir` as the `-cg` directory of this build: if the process
// ends while a directory is still set aside for it, the exit hook gives that one back.
fn v3_watch_c_debug_dir(cc_dir string) {
	v3_watch_c_debug_dir_as(cc_dir, c_debug_dir_armed)
}

// v3_watch_c_debug_dir_as is v3_watch_c_debug_dir with the state the signal word is left at:
// `setting aside` for a build that is about to move the directory and does not know yet
// whether it can.
fn v3_watch_c_debug_dir_as(cc_dir string, state u64) {
	if cc_dir.len == 0 || cc_dir.len >= max_c_debug_dir_path {
		v3_c_debug_dir_watched_len = 0
		v3_disarm_c_debug_dir_signals()
		return
	}
	// An interrupt does not run the exit hook: see c_debug_dir_signal.c.v.
	v3_arm_c_debug_dir_signals(cc_dir, state)
	for i in 0 .. cc_dir.len {
		v3_c_debug_dir_watched[i] = cc_dir[i]
	}
	v3_c_debug_dir_watched_len = cc_dir.len
	v3_c_debug_dir_watched_pid = os.getpid()
	if !v3_c_debug_dir_hook_registered {
		v3_c_debug_dir_hook_registered = true
		at_exit(v3_give_back_c_debug_dir_at_exit) or {}
	}
}

// v3_unwatch_c_debug_dir is called when nothing is set aside any more.
fn v3_unwatch_c_debug_dir() {
	v3_c_debug_dir_watched_len = 0
	v3_disarm_c_debug_dir_signals()
}

// v3_watched_c_debug_dir returns the directory the exit hook looks after, or '' for none.
fn v3_watched_c_debug_dir() string {
	if v3_c_debug_dir_watched_len == 0 {
		return ''
	}
	mut path := []u8{cap: v3_c_debug_dir_watched_len}
	for i in 0 .. v3_c_debug_dir_watched_len {
		path << v3_c_debug_dir_watched[i]
	}
	return path.bytestr()
}

// v3_give_back_c_debug_dir_at_exit runs when the process ends. A directory that is still set
// aside then belongs to the executable in place: no new one replaced it, in this process or in
// a build it started. A build that succeeded, or a failure that already went through
// cleanup_c_build_dir, has left nothing set aside, and nothing is done.
fn v3_give_back_c_debug_dir_at_exit() {
	// A forked child must not take the directory of the build that started it.
	if os.getpid() != v3_c_debug_dir_watched_pid {
		return
	}
	cc_dir := v3_watched_c_debug_dir()
	if cc_dir.len > 0 && v3_c_debug_dir_is_set_aside(cc_dir) {
		// cleanup_c_build_dir takes the directories for the build before it touches them
		// (see v3_begin_c_debug_dir_rollback): a signal in the middle of that waits and is
		// raised when the directory is back, and a handler that has them already keeps
		// them. It ends the watch itself once the directory is back.
		cleanup_c_build_dir(cc_dir)
	}
	v3_unwatch_c_debug_dir()
}

// v3_give_back_c_debug_dir_as_owner is what a signal handler calls on Windows, where it runs
// on a thread of its own: it HAS the directories (the word is `giving back`), so it gives
// back without asking for them, which cleanup_c_build_dir would wait for without end.
fn v3_give_back_c_debug_dir_as_owner() {
	if os.getpid() != v3_c_debug_dir_watched_pid {
		return
	}
	cc_dir := v3_watched_c_debug_dir()
	if cc_dir.len > 0 && v3_c_debug_dir_is_set_aside(cc_dir) {
		v3_give_back_c_build_dir(cc_dir)
	}
}
