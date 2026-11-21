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

// v3_watch_c_debug_dir records `cc_dir` as the `-cg` directory of this build: if the process
// ends while a directory is still set aside for it, the exit hook gives that one back.
fn v3_watch_c_debug_dir(cc_dir string) {
	if cc_dir.len == 0 || cc_dir.len >= max_c_debug_dir_path {
		v3_c_debug_dir_watched_len = 0
		return
	}
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
	v3_c_debug_dir_watched_len = 0
	if cc_dir.len > 0 && os.is_dir(v3_c_debug_dir_set_aside(cc_dir)) {
		cleanup_c_build_dir(cc_dir)
	}
}
