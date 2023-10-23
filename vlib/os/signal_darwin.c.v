[has_globals]
module os

#include <signal.h>

// A convenient way to ignore certain system signals when there is no need to process certain system signals,
// Masking of system signals under posix systems requires a distinction between main and background threads.
// Since there is no good way to easily tell whether the current thread is the main or background thread,
// So a global variable is introduced to make the distinction.

fn C.pthread_self() usize

__global main_thread_id = u64(C.pthread_self())

fn is_main_thread() bool {
	return main_thread_id == u64(C.pthread_self())
}

fn C.sigaddset(set &u32, signum int) int
fn C.sigemptyset(set &u32)
fn C.sigprocmask(how int, set &u32, oldset &u32) int

// signal_ignore to mask system signals, e.g.: signal_ignore(.pipe, .urg, ...)
pub fn signal_ignore(args ...Signal) {
	if is_main_thread() {
		// for main thread.
		for arg in args {
			C.signal(int(arg), ignore_signal_handler)
		}
	} else {
		// for background threads.
		mask1 := u32(0)
		C.sigemptyset(&mask1)
		for arg in args {
			C.sigaddset(&mask1, int(arg))
		}
		C.sigprocmask(C.SIG_BLOCK, &mask1, unsafe { nil })
	}
}
