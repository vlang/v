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
//   armed        the old executable is in place: a signal gives the directory back
//   giving back  a handler is doing that; the build must not put its executable in place,
//                and must not give the directory back itself
//   rolling back the build is giving the directory back (a failure, or the exit hook); a
//                signal waits until that is done
//   publishing   the build is putting its executable in place; a signal waits until it knows
//   idle         nothing is set aside for this process: a signal only ends it
// and, while the state is `publishing` or `rolling back`, the bits above it are the signal
// that waits. A signal is noted and the state is left in ONE step: either the handler's swap
// puts its signal into the word before the build's swap takes the word out of that state, and
// the build finds it there, or the handler's swap fails, and it looks at the word again. A
// swap that fails is never a reason to act: whoever fails reads the word again and decides
// from what it is now.
//
// The directories are touched by whoever took the word out of `armed`, and by nobody else:
// the handler (giving back), or the build (rolling back, or publishing). Two rollbacks that
// overlapped undid each other: the build removed the sources the handler had just put back.
const c_debug_dir_idle = u64(0)
const c_debug_dir_armed = u64(1)
const c_debug_dir_giving_back = u64(2)
const c_debug_dir_publishing = u64(3)
const c_debug_dir_rolling_back = u64(4)
const c_debug_dir_state_mask = u64(0xff)
const c_debug_dir_signal_shift = 8

// What a signal handler is to do, from the word it met.
enum CDebugDirSignalStep {
	again     // the word changed meanwhile: look at it again
	end       // nothing is set aside: end the process as the signal would
	give_back // this handler has the directory: give it back, then end the process
	wait      // the build is publishing or rolling back and has the signal now: return
	stand_by  // another handler is giving the directory back and will end the process: return
}

__global v3_c_debug_dir_path [4200]u8
__global v3_c_debug_dir_path_set_aside [4200]u8
__global v3_c_debug_dir_path_interrupted [4200]u8
__global v3_c_debug_dir_path_retired [4200]u8
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

// v3_c_debug_dir_retired is the mark beside a set-aside directory that says: this is NOT the
// sources of the executable in place any more. A build that put a new executable in place
// renames what it had set aside (to the name of something to remove); when that cannot be
// done, it leaves this mark instead, so that the directory is under the name of something to
// give back only together with the word that it is not. Everything that would give a
// set-aside directory back, or take one for the sources of the executable in place, looks
// for the mark first.
fn v3_c_debug_dir_retired(cc_dir string) string {
	return v3_c_debug_dir_set_aside(cc_dir) + '.retired'
}

// v3_c_debug_dir_is_set_aside tells whether `cc_dir` has a directory set aside that is still
// the sources of the executable in place.
fn v3_c_debug_dir_is_set_aside(cc_dir string) bool {
	return os.is_dir(v3_c_debug_dir_set_aside(cc_dir)) && !os.exists(v3_c_debug_dir_retired(cc_dir))
}

// v3_drop_retired_c_debug_dir removes a set-aside directory that was retired, and then its
// mark. It returns false while the directory or the mark is still there: the name of a
// set-aside directory cannot be used for a new one then.
fn v3_drop_retired_c_debug_dir(cc_dir string) bool {
	retired := v3_c_debug_dir_retired(cc_dir)
	if !os.exists(retired) {
		return true
	}
	set_aside := v3_c_debug_dir_set_aside(cc_dir)
	os.rmdir_all(set_aside) or {}
	if os.exists(set_aside) {
		return false
	}
	// the mark last: a directory under that name without its mark would be given back
	os.rm(retired) or {}
	return !os.exists(retired)
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
	v3_write_c_debug_dir_path(&v3_c_debug_dir_path_retired[0], v3_c_debug_dir_retired(cc_dir))
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

// v3_take_c_debug_dir_word ends the state `held` (publishing or rolling back) in one step,
// to `next`, and returns the signal that waited in the word (0 for none). It returns -1 when
// the word was not in that state.
fn v3_take_c_debug_dir_word(held u64, next u64) int {
	for {
		word := v3_c_debug_dir_load()
		if word & c_debug_dir_state_mask != held {
			return -1
		}
		if v3_c_debug_dir_swap(word, next) {
			return int(word >> c_debug_dir_signal_shift)
		}
	}
	return -1
}

// v3_take_c_debug_dir_publication ends the publication state in one step, to `next`, and
// returns the signal that waited in the word (0 for none). It returns -1 when the word was
// not in the publication state (no publication was begun).
fn v3_take_c_debug_dir_publication(next u64) int {
	return v3_take_c_debug_dir_word(c_debug_dir_publishing, next)
}

// What the build is to do before it gives a directory back, from the word it met.
enum CDebugDirRollbackStep {
	again   // the word changed meanwhile: look at it again
	mine    // the build has the directories now: give back, then v3_end_c_debug_dir_rollback
	theirs  // a handler has them and is ending the process: touch nothing, wait for the end
	busy    // another thread of the build is rolling back or publishing: wait, look again
	nobodys // nothing is set aside for this process: no handler touches the directories
}

// v3_c_debug_dir_rollback_step decides that from `word`, the word as the build read it, and
// takes the word accordingly in one swap.
fn v3_c_debug_dir_rollback_step(word u64) CDebugDirRollbackStep {
	match word & c_debug_dir_state_mask {
		c_debug_dir_armed {
			if v3_c_debug_dir_swap(word, c_debug_dir_rolling_back) {
				return .mine
			}
			return .again
		}
		c_debug_dir_giving_back {
			return .theirs
		}
		c_debug_dir_rolling_back, c_debug_dir_publishing {
			return .busy
		}
		else {
			return .nobodys
		}
	}
}

// v3_begin_c_debug_dir_rollback is called by the build before it removes the watched
// directory and moves back what was set aside. It returns true when the build has the
// directories (v3_end_c_debug_dir_rollback is then to follow), and false when nothing is set
// aside for this process, so that no handler would touch them. When a handler is giving the
// directory back, the process is about to end by its signal and the directories are not the
// build's to touch: this does not return then, as v3_begin_c_debug_dir_publication does not.
fn v3_begin_c_debug_dir_rollback() bool {
	for {
		match v3_c_debug_dir_rollback_step(v3_c_debug_dir_load()) {
			.mine { return true }
			.nobodys { return false }
			.again {}
			.theirs, .busy { time.sleep(10 * time.millisecond) }
		}
	}
	return false
}

// v3_end_c_debug_dir_rollback is called when the build has given the directory back, or found
// nothing to give back: the word goes to `next` (idle when nothing is set aside any more,
// armed when the watch goes on), and a signal that waited meanwhile is raised, now that it
// finds the directories at rest.
fn v3_end_c_debug_dir_rollback(next u64) {
	waiting := v3_take_c_debug_dir_word(c_debug_dir_rolling_back, next)
	if waiting > 0 {
		C.raise(i32(waiting))
	}
}

// v3_retire_c_debug_dir_set_aside is called when the new executable is in place, still inside
// the publication (a signal waits): what was set aside is the sources of nothing any more,
// and must not be found under the name of something to give back when the publication is
// over. One rename does that. When the rename fails (a file in the directory is open, on
// Windows), the directory keeps its name and gets the mark that says it was retired; when
// the mark cannot be written either, the directory is removed here and now, with the signal
// still waiting. It returns what is left to remove after the publication ('' for nothing).
// A directory that could be neither renamed, marked nor removed is said aloud: it is the one
// case in which a later build that fails would give back the wrong sources.
fn v3_retire_c_debug_dir_set_aside(cc_dir string) string {
	set_aside := v3_c_debug_dir_set_aside(cc_dir)
	if !os.is_dir(set_aside) {
		return ''
	}
	interrupted := v3_c_debug_dir_interrupted(cc_dir)
	if !os.exists(interrupted) {
		os.rename(set_aside, interrupted) or {}
		if !os.exists(set_aside) {
			return interrupted
		}
	}
	os.write_file(v3_c_debug_dir_retired(cc_dir), '') or {}
	if os.exists(v3_c_debug_dir_retired(cc_dir)) {
		return set_aside
	}
	for _ in 0 .. 5 {
		os.rmdir_all(set_aside) or {}
		if !os.exists(set_aside) {
			return ''
		}
		time.sleep(20 * time.millisecond)
	}
	eprintln('warning: -cg could not rename, mark or remove ${set_aside}, the C sources of the executable that was just replaced. Remove it by hand: while it is there, a later -cg build of this output that fails takes it for the sources of the executable in place.')
	return ''
}

// v3_end_c_debug_dir_publication is called when the move is over. When the new executable is
// in place (`published`), the directory that was set aside belongs to nothing any more: it
// gets the name of something to remove, in one rename, so that no later moment finds it under
// the name of something to give back, and only then is it removed. When the move failed, the
// old executable is still in place and the directory is to be given back as before. A signal
// that arrived meanwhile is raised again now that it can be answered.
fn v3_end_c_debug_dir_publication(cc_dir string, published bool) {
	// What was set aside is retired BEFORE the publication is over, and by something that
	// does not depend on its removal: a signal that waited is raised as soon as the word is
	// taken, and the removal below may then never run, or may fail.
	replaced := if published { v3_retire_c_debug_dir_set_aside(cc_dir) } else { '' }
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
	if published {
		// a retired directory that can go now goes with its mark, and so does a mark that
		// an earlier build left for a directory that this one could rename after all
		v3_drop_retired_c_debug_dir(cc_dir)
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
		c_debug_dir_publishing, c_debug_dir_rolling_back {
			// The build is moving its executable into place and does not know yet which
			// sources belong to the one that will be there; or it is giving the directory
			// back itself, and the directories are in between. It takes this signal out of
			// the word when it is done, and raises it.
			state := word & c_debug_dir_state_mask
			if v3_c_debug_dir_swap(word, state | (u64(sig) << c_debug_dir_signal_shift)) {
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

// v3_give_back_c_debug_dir_in_a_handler is the giving back of the Unix handler, which can
// have interrupted this process anywhere: access() and rename() on the paths written when
// the directory was set aside, and nothing else. Only when something is set aside does the
// first rename have a second to follow: rename() of a directory that is not there fails and
// changes nothing. And only when it was not retired: a directory with the mark beside it is
// the sources of an executable that was replaced.
fn v3_give_back_c_debug_dir_in_a_handler() {
	$if !windows {
		if C.access(&char(&v3_c_debug_dir_path_set_aside[0]), 0) == 0
			&& C.access(&char(&v3_c_debug_dir_path_retired[0]), 0) != 0 {
			C.rename(&char(&v3_c_debug_dir_path[0]), &char(&v3_c_debug_dir_path_interrupted[0]))
			C.rename(&char(&v3_c_debug_dir_path_set_aside[0]), &char(&v3_c_debug_dir_path[0]))
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
			v3_give_back_c_debug_dir_as_owner()
		} $else {
			v3_give_back_c_debug_dir_in_a_handler()
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
