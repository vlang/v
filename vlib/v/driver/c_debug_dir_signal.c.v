@[has_globals]
module driver

import os

fn C.raise(sig i32) i32

// Ctrl-C ends the compiler without running its exit handlers, so the exit hook of
// c_debug_dir_exit.v does not give a set-aside `-cg` directory back. These handlers do, for
// SIGINT, SIGTERM and SIGHUP, and then let the signal end the process as it would have.
//
// A handler can interrupt this process anywhere, in the allocator too, so on Unix it does no
// more than two rename() calls on paths that were written out when the directory was set
// aside: the unfinished directory moves to `<dir>.interrupted` (the next `-cg` build of the
// output removes it), and the one set aside moves back. On Windows the C runtime calls the
// handler on a thread of its own, where the ordinary cleanup is safe.
__global v3_c_debug_dir_path [4200]u8
__global v3_c_debug_dir_path_set_aside [4200]u8
__global v3_c_debug_dir_path_interrupted [4200]u8
__global v3_c_debug_dir_signals_pid int
__global v3_c_debug_dir_signals_installed bool
__global v3_c_debug_dir_previous_int os.SignalHandler
__global v3_c_debug_dir_previous_term os.SignalHandler
__global v3_c_debug_dir_previous_hup os.SignalHandler

// v3_c_debug_dir_interrupted is where an interrupted `-cg` build leaves what it had written.
fn v3_c_debug_dir_interrupted(cc_dir string) string {
	return cc_dir + '.interrupted'
}

// v3_write_c_debug_dir_path writes `path` and its terminating zero to `buffer`.
fn v3_write_c_debug_dir_path(buffer &u8, path string) {
	unsafe {
		for i in 0 .. path.len {
			buffer[i] = path[i]
		}
		buffer[path.len] = 0
	}
}

// v3_arm_c_debug_dir_signals makes an interrupt of this build give `cc_dir` back. Called when
// the directory is set aside; `cc_dir` is shorter than max_c_debug_dir_path.
fn v3_arm_c_debug_dir_signals(cc_dir string) {
	v3_c_debug_dir_signals_pid = 0
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path[0], cc_dir)
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_set_aside[0], v3_c_debug_dir_set_aside(cc_dir))
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_interrupted[0], v3_c_debug_dir_interrupted(cc_dir))
	v3_c_debug_dir_signals_pid = os.getpid()
	if v3_c_debug_dir_signals_installed {
		return
	}
	v3_c_debug_dir_signals_installed = true
	v3_c_debug_dir_previous_int = v3_install_c_debug_dir_signal(.int)
	v3_c_debug_dir_previous_term = v3_install_c_debug_dir_signal(.term)
	$if !windows {
		v3_c_debug_dir_previous_hup = v3_install_c_debug_dir_signal(.hup)
	}
}

// v3_disarm_c_debug_dir_signals is called when nothing is set aside any more: an interrupt
// then only ends the process.
fn v3_disarm_c_debug_dir_signals() {
	v3_c_debug_dir_signals_pid = 0
}

// v3_install_c_debug_dir_signal installs the handler for `sig` and returns what was there. A
// signal that this process was started to ignore (nohup) stays ignored: giving the directory
// back and then carrying on with the build would leave the new executable with the old sources.
fn v3_install_c_debug_dir_signal(sig os.Signal) os.SignalHandler {
	previous := os.signal_opt(sig, v3_c_debug_dir_signal) or { return unsafe { nil } }
	if voidptr(previous) == voidptr(C.SIG_IGN) {
		os.signal_opt(sig, previous) or {}
	}
	return previous
}

fn v3_c_debug_dir_signal(sig os.Signal) {
	if v3_c_debug_dir_signals_pid != 0 && os.getpid() == v3_c_debug_dir_signals_pid {
		v3_c_debug_dir_signals_pid = 0
		$if windows {
			v3_give_back_c_debug_dir_at_exit()
		} $else {
			// Only when something is set aside does the first rename have a second to follow:
			// rename() of a directory that is not there fails and changes nothing.
			if C.access(&char(&v3_c_debug_dir_path_set_aside[0]), 0) == 0 {
				C.rename(&char(&v3_c_debug_dir_path[0]), &char(&v3_c_debug_dir_path_interrupted[0]))
				C.rename(&char(&v3_c_debug_dir_path_set_aside[0]), &char(&v3_c_debug_dir_path[0]))
			}
		}
	}
	// End as the signal would have ended the process: what was installed before, raised again.
	previous := match sig {
		.int { v3_c_debug_dir_previous_int }
		.term { v3_c_debug_dir_previous_term }
		else { v3_c_debug_dir_previous_hup }
	}
	os.signal_opt(sig, previous) or {}
	C.raise(i32(sig))
}
