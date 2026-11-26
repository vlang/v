module progress

import os
import term.termios

#include <signal.h>

// Query the disposition without replacing it. Installing the pointer returned
// by signal() can install a sanitizer's interceptor as its own handler.
#define v_progress_signal_is_default(sig, old) (sigaction((sig), NULL, (old)) == 0 && (old)->sa_handler == SIG_DFL)

fn C.v_progress_signal_is_default(sig i32, old &C.sigaction) bool

fn signal_is_default(sig os.Signal) bool {
	old := C.sigaction{}
	return C.v_progress_signal_is_default(int(sig), &old)
}

// terminal_size returns the columns and rows of the terminal attached to
// `fd`, or (0, 0) when it cannot be determined.
//
// term.get_terminal_size() is not used because it always asks about stdout
// (fd 1), while the bars draw on stderr by default. The ioctl wrapper and the
// C.winsize declaration are the standard library's own, via term and termios.
fn terminal_size(fd int) (int, int) {
	ws := C.winsize{}
	if termios.ioctl(fd, u64(termios.flag(C.TIOCGWINSZ)), voidptr(&ws)) != 0 {
		return 0, 0
	}
	return int(ws.ws_col), int(ws.ws_row)
}
