module term

import os

#include <sys/ioctl.h>

#include <termios.h> // TIOCGWINSZ

pub struct C.winsize {
pub:
	ws_row    u16
	ws_col    u16
	ws_xpixel u16
	ws_ypixel u16
}

fn C.ioctl(fd int, request u64, arg voidptr) int

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int,int) {
	if is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return default_columns_size, default_rows_size
	}
	w := C.winsize{}
	C.ioctl(1, C.TIOCGWINSZ, &w)
	return int(w.ws_col),int(w.ws_row)
}
