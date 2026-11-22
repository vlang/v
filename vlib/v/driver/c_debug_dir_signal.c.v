@[has_globals]
module driver

import os
import sync.stdatomic
import time

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
//
// Whether the directory is to be given back depends on which executable is in place, and that
// changes at one moment: when the new executable is moved over the old one. One word of state
// says who may act, the handler or the build, and each takes it with a compare-and-swap:
//   armed       the old executable is in place: a signal gives the directory back
//   giving back a handler is doing that; the build must not put its executable in place
//   publishing  the build is putting its executable in place; a signal waits until it knows
//   idle        nothing is set aside for this process: a signal only ends it
const c_debug_dir_idle = u64(0)
const c_debug_dir_armed = u64(1)
const c_debug_dir_giving_back = u64(2)
const c_debug_dir_publishing = u64(3)

__global v3_c_debug_dir_path [4200]u8
__global v3_c_debug_dir_path_set_aside [4200]u8
__global v3_c_debug_dir_path_interrupted [4200]u8
__global v3_c_debug_dir_state u64
__global v3_c_debug_dir_signals_pid int
__global v3_c_debug_dir_signal_waiting int
__global v3_c_debug_dir_signals_installed bool
__global v3_c_debug_dir_previous_int os.SignalHandler
__global v3_c_debug_dir_previous_term os.SignalHandler
__global v3_c_debug_dir_previous_hup os.SignalHandler

// v3_c_debug_dir_interrupted is where an interrupted `-cg` build leaves what it had written,
// and where a build that succeeded puts the directory it replaced until it has removed it.
fn v3_c_debug_dir_interrupted(cc_dir string) string {
	return cc_dir + '.interrupted'
}

// v3_c_debug_dir_state_is reads the state.
fn v3_c_debug_dir_state_is(state u64) bool {
	return stdatomic.load_u64(&v3_c_debug_dir_state) == state
}

// v3_c_debug_dir_state_from changes the state from `from` to `to`, when it is `from`, and
// says whether it did.
fn v3_c_debug_dir_state_from(from u64, to u64) bool {
	mut expected := from
	return C.atomic_compare_exchange_strong_u64(voidptr(&v3_c_debug_dir_state), &expected, to)
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
	stdatomic.store_u64(&v3_c_debug_dir_state, c_debug_dir_idle)
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path[0], cc_dir)
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_set_aside[0], v3_c_debug_dir_set_aside(cc_dir))
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_interrupted[0], v3_c_debug_dir_interrupted(cc_dir))
	v3_c_debug_dir_signals_pid = os.getpid()
	v3_c_debug_dir_signal_waiting = 0
	if !v3_c_debug_dir_signals_installed {
		v3_c_debug_dir_signals_installed = true
		v3_c_debug_dir_previous_int = v3_install_c_debug_dir_signal(.int)
		v3_c_debug_dir_previous_term = v3_install_c_debug_dir_signal(.term)
		$if !windows {
			v3_c_debug_dir_previous_hup = v3_install_c_debug_dir_signal(.hup)
		}
	}
	stdatomic.store_u64(&v3_c_debug_dir_state, c_debug_dir_armed)
}

// v3_disarm_c_debug_dir_signals is called when nothing is set aside any more: an interrupt
// then only ends the process. A handler that is giving the directory back keeps the state.
fn v3_disarm_c_debug_dir_signals() {
	v3_c_debug_dir_state_from(c_debug_dir_armed, c_debug_dir_idle)
}

// v3_begin_c_debug_dir_publication is called before the new executable is moved into place.
// From here to v3_end_c_debug_dir_publication a signal neither gives the directory back nor
// ends the process: it waits. When a handler is already giving the directory back, the
// process is about to end by its signal, and the executable must not be moved any more: this
// does not return then.
fn v3_begin_c_debug_dir_publication() {
	if v3_c_debug_dir_state_from(c_debug_dir_armed, c_debug_dir_publishing) {
		return
	}
	for v3_c_debug_dir_state_is(c_debug_dir_giving_back) {
		time.sleep(10 * time.millisecond)
	}
}

// v3_end_c_debug_dir_publication is called when the move is over. When the new executable is
// in place (`published`), the directory that was set aside belongs to nothing any more: it
// gets the name of something to remove, in one rename, so that no later moment finds it under
// the name of something to give back, and only then is it removed. When the move failed, the
// old executable is still in place and the directory is to be given back as before. A signal
// that arrived meanwhile is raised again now that it can be answered.
fn v3_end_c_debug_dir_publication(cc_dir string, published bool) {
	if !v3_c_debug_dir_state_is(c_debug_dir_publishing) {
		if published {
			os.rmdir_all(v3_c_debug_dir_set_aside(cc_dir)) or {}
		}
		return
	}
	mut replaced := ''
	if published {
		set_aside := v3_c_debug_dir_set_aside(cc_dir)
		if os.is_dir(set_aside) {
			replaced = v3_c_debug_dir_interrupted(cc_dir)
			os.rename(set_aside, replaced) or {
				// It cannot be renamed (a file in it is open, on Windows): removing it under
				// its own name is what is left.
				replaced = set_aside
			}
		}
		stdatomic.store_u64(&v3_c_debug_dir_state, c_debug_dir_idle)
	} else {
		stdatomic.store_u64(&v3_c_debug_dir_state, c_debug_dir_armed)
	}
	waiting := v3_c_debug_dir_signal_waiting
	if waiting != 0 {
		v3_c_debug_dir_signal_waiting = 0
		C.raise(i32(waiting))
	}
	if replaced.len > 0 {
		os.rmdir_all(replaced) or {}
	}
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
	if os.getpid() == v3_c_debug_dir_signals_pid {
		if v3_c_debug_dir_state_is(c_debug_dir_publishing) {
			// The build is moving its executable into place and does not know yet which
			// sources belong to the one that will be there. It raises this again when it does.
			v3_c_debug_dir_signal_waiting = int(sig)
			$if windows {
				// the C runtime has put the default back before calling this
				os.signal_opt(sig, v3_c_debug_dir_signal) or {}
			}
			return
		}
		if v3_c_debug_dir_state_from(c_debug_dir_armed, c_debug_dir_giving_back) {
			$if windows {
				v3_give_back_c_debug_dir_at_exit()
			} $else {
				// Only when something is set aside does the first rename have a second to
				// follow: rename() of a directory that is not there fails and changes nothing.
				if C.access(&char(&v3_c_debug_dir_path_set_aside[0]), 0) == 0 {
					C.rename(&char(&v3_c_debug_dir_path[0]), &char(&v3_c_debug_dir_path_interrupted[0]))
					C.rename(&char(&v3_c_debug_dir_path_set_aside[0]), &char(&v3_c_debug_dir_path[0]))
				}
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
