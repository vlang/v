module os

#include <signal.h>

fn C.signal(signal int, handlercb SignalHandler) voidptr

// signal will assign `handler` callback to be called when `signum` signal is received.
pub fn signal_opt(signum Signal, handler SignalHandler) !SignalHandler {
	C.errno = 0
	prev_handler := C.signal(int(signum), handler)
	if prev_handler == C.SIG_ERR {
		// errno isn't correctly set on Windows, but EINVAL is this only possible value it can take anyway
		return error_with_code(posix_get_error_msg(C.EINVAL), C.EINVAL)
	}
	return SignalHandler(prev_handler)
}

// A convenient way to ignore certain system signals when there is no need to process certain system signals,
// Masking of system signals under posix systems requires a distinction between main and background threads.
// Since there is no good way to easily tell whether the current thread is the main or background thread,
// So a global variable is introduced to make the distinction.

// An empty system signal handler (callback function) used to mask the specified system signal.
fn ignore_signal_handler(signal Signal) {
}

// signal_ignore to mask system signals, e.g.: signal_ignore(.pipe, .urg, ...)
pub fn signal_ignore(args ...Signal) {
	if is_main_thread() {
		// for main thread.
		$if !windows {
			for arg in args {
				signal_opt(arg, ignore_signal_handler) or {}
			}
		}
	} else {
		// for background threads.
		$if macos {
			mask1 := u32(0)
			C.sigemptyset(&mask1)
			for arg in args {
				C.sigaddset(&mask1, int(arg))
			}
			C.sigprocmask(C.SIG_BLOCK, &mask1, unsafe { nil })
		}
		$if !windows && !macos && !andorid {
			mask1 := C.sigset_t{}
			C.sigemptyset(&mask1)
			for arg in args {
				C.sigaddset(&mask1, int(arg))
			}
			C.sigprocmask(C.SIG_BLOCK, &mask1, unsafe { nil })
		}
	}
}
