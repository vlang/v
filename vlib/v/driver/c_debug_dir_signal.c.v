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
// changes at one moment: when the new executable is moved over the old one. A handler can run
// on another thread than the build (always, on Windows), so the two agree through ONE word,
// changed by compare-and-swap only. Its low byte is the state:
//   armed       the old executable is in place: a signal gives the directory back
//   giving back a handler is doing that; the build must not put its executable in place
//   publishing  the build is putting its executable in place; a signal waits until it knows
//   idle        nothing is set aside for this process: a signal only ends it
// and, while the state is `publishing`, the bits above it are the signal that waits. A signal
// is noted and the state is left in ONE step: either the handler's swap puts its signal into
// the word before the build's swap takes the word out of `publishing`, and the build finds
// it there, or the handler's swap fails, and it looks at the word again. A swap that fails is
// never a reason to act: whoever fails reads the word again and decides from what it is now.
const c_debug_dir_idle = u64(0)
const c_debug_dir_armed = u64(1)
const c_debug_dir_giving_back = u64(2)
const c_debug_dir_publishing = u64(3)
const c_debug_dir_state_mask = u64(0xff)
const c_debug_dir_signal_shift = 8

// What a signal handler is to do, from the word it met.
enum CDebugDirSignalStep {
	again     // the word changed meanwhile: look at it again
	end       // nothing is set aside: end the process as the signal would
	give_back // this handler has the directory: give it back, then end the process
	wait      // the build is publishing and has the signal now: return
	stand_by  // another handler is giving the directory back and will end the process: return
}

__global v3_c_debug_dir_path [4200]u8
__global v3_c_debug_dir_path_set_aside [4200]u8
__global v3_c_debug_dir_path_interrupted [4200]u8
__global v3_c_debug_dir_word u64
__global v3_c_debug_dir_signals_pid int
__global v3_c_debug_dir_signals_installed bool
__global v3_c_debug_dir_previous_int os.SignalHandler
__global v3_c_debug_dir_previous_term os.SignalHandler
__global v3_c_debug_dir_previous_hup os.SignalHandler

// v3_c_debug_dir_interrupted is where an interrupted `-cg` build leaves what it had written,
// and where a build that succeeded puts the directory it replaced until it has removed it.
fn v3_c_debug_dir_interrupted(cc_dir string) string {
	return cc_dir + '.interrupted'
}

// v3_c_debug_dir_load reads the word.
fn v3_c_debug_dir_load() u64 {
	return stdatomic.load_u64(&v3_c_debug_dir_word)
}

// v3_c_debug_dir_swap changes the word from `from` to `to`, when it is `from`, and says
// whether it did.
fn v3_c_debug_dir_swap(from u64, to u64) bool {
	mut expected := from
	return C.atomic_compare_exchange_strong_u64(voidptr(&v3_c_debug_dir_word), &expected, to)
}

// v3_c_debug_dir_state_is reads the state.
fn v3_c_debug_dir_state_is(state u64) bool {
	return v3_c_debug_dir_load() & c_debug_dir_state_mask == state
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
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_idle)
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path[0], cc_dir)
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_set_aside[0], v3_c_debug_dir_set_aside(cc_dir))
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_interrupted[0], v3_c_debug_dir_interrupted(cc_dir))
	v3_c_debug_dir_signals_pid = os.getpid()
	if !v3_c_debug_dir_signals_installed {
		v3_c_debug_dir_signals_installed = true
		v3_c_debug_dir_previous_int = v3_install_c_debug_dir_signal(.int)
		v3_c_debug_dir_previous_term = v3_install_c_debug_dir_signal(.term)
		$if !windows {
			v3_c_debug_dir_previous_hup = v3_install_c_debug_dir_signal(.hup)
		}
	}
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_armed)
}

// v3_disarm_c_debug_dir_signals is called when nothing is set aside any more: an interrupt
// then only ends the process. A handler that is giving the directory back, or a publication
// in progress, keeps the word.
fn v3_disarm_c_debug_dir_signals() {
	v3_c_debug_dir_swap(c_debug_dir_armed, c_debug_dir_idle)
}

// v3_begin_c_debug_dir_publication is called before the new executable is moved into place.
// From here to v3_end_c_debug_dir_publication a signal neither gives the directory back nor
// ends the process: it waits. When a handler is already giving the directory back, the
// process is about to end by its signal, and the executable must not be moved any more: this
// does not return then.
fn v3_begin_c_debug_dir_publication() {
	if v3_c_debug_dir_swap(c_debug_dir_armed, c_debug_dir_publishing) {
		return
	}
	for v3_c_debug_dir_state_is(c_debug_dir_giving_back) {
		time.sleep(10 * time.millisecond)
	}
}

// v3_take_c_debug_dir_publication ends the publication state in one step, to `next`, and
// returns the signal that waited in the word (0 for none). It returns -1 when the word was
// not in the publication state (no publication was begun).
fn v3_take_c_debug_dir_publication(next u64) int {
	for {
		word := v3_c_debug_dir_load()
		if word & c_debug_dir_state_mask != c_debug_dir_publishing {
			return -1
		}
		if v3_c_debug_dir_swap(word, next) {
			return int(word >> c_debug_dir_signal_shift)
		}
	}
	return -1
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
	}
	waiting := v3_take_c_debug_dir_publication(if published {
		c_debug_dir_idle
	} else {
		c_debug_dir_armed
	})
	if waiting > 0 {
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

// v3_c_debug_dir_signal_step decides what a handler of `sig` is to do from `word`, the word
// as the handler read it, and takes the word accordingly in one swap. When the word is not
// `word` any more, nothing is taken and nothing is decided: `.again`.
fn v3_c_debug_dir_signal_step(word u64, sig int) CDebugDirSignalStep {
	match word & c_debug_dir_state_mask {
		c_debug_dir_armed {
			if v3_c_debug_dir_swap(word, c_debug_dir_giving_back) {
				return .give_back
			}
			return .again
		}
		c_debug_dir_publishing {
			// The build is moving its executable into place and does not know yet which
			// sources belong to the one that will be there. It takes this signal out of the
			// word when it does, and raises it.
			if v3_c_debug_dir_swap(word, c_debug_dir_publishing | (u64(sig) << c_debug_dir_signal_shift)) {
				return .wait
			}
			return .again
		}
		c_debug_dir_giving_back {
			return .stand_by
		}
		else {
			return .end
		}
	}
}

fn v3_c_debug_dir_signal(sig os.Signal) {
	mut step := CDebugDirSignalStep.end
	if os.getpid() == v3_c_debug_dir_signals_pid {
		step = .again
		for step == .again {
			step = v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(sig))
		}
	}
	if step == .wait || step == .stand_by {
		$if windows {
			// the C runtime has put the default back before calling this
			os.signal_opt(sig, v3_c_debug_dir_signal) or {}
		}
		return
	}
	if step == .give_back {
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
	// End as the signal would have ended the process: what was installed before, raised again.
	previous := match sig {
		.int { v3_c_debug_dir_previous_int }
		.term { v3_c_debug_dir_previous_term }
		else { v3_c_debug_dir_previous_hup }
	}
	os.signal_opt(sig, previous) or {}
	C.raise(i32(sig))
}
