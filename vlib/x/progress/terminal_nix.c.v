module progress

import term.termios

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
