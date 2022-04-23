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

fn C.tcgetattr(fd int, ptr &C.termios) int

fn C.tcsetattr(fd int, action int, const_ptr &C.termios)

fn C.ioctl(fd int, request u64, arg voidptr) int

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return default_columns_size, default_rows_size
	}
	w := C.winsize{}
	unsafe { C.ioctl(1, u64(C.TIOCGWINSZ), &w) }
	return int(w.ws_col), int(w.ws_row)
}

// get_cursor_position returns a Coord containing the current cursor position
pub fn get_cursor_position() ?Coord {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return Coord{0, 0}
	}

	old_state := C.termios{}
	if unsafe { C.tcgetattr(0, &old_state) } != 0 {
		return os.last_error()
	}
	defer {
		// restore the old terminal state:
		unsafe { C.tcsetattr(0, C.TCSANOW, &old_state) }
	}

	mut state := C.termios{}
	if unsafe { C.tcgetattr(0, &state) } != 0 {
		return os.last_error()
	}
	state.c_lflag &= int(~(u32(C.ICANON) | u32(C.ECHO)))
	unsafe { C.tcsetattr(0, C.TCSANOW, &state) }

	print('\e[6n')

	mut x := 0
	mut y := 0
	mut stage := u8(0)

	// ESC [ YYY `;` XXX `R`

	for {
		w := unsafe { C.getchar() }
		if w < 0 {
			return error_with_code('Failed to read from stdin', 888)
		} else if w == `[` || w == `;` {
			stage++
		} else if `0` <= w && w <= `9` {
			match stage {
				// converting string values to int:
				1 { y = y * 10 + int(w - `0`) }
				2 { x = x * 10 + int(w - `0`) }
				else {}
			}
		} else if w == `R` {
			break
		}
	}
	return Coord{x, y}
}

// set_terminal_title change the terminal title
pub fn set_terminal_title(title string) bool {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return true
	}
	print('\033]0')
	print(title)
	print('\007')
	return true
}

// clear clears current terminal screen.
pub fn clear() {
	print('\x1b[2J')
	print('\x1b[H')
}
