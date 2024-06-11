module term

import os
import strings
import term.termios

#include <sys/ioctl.h>

pub struct C.winsize {
pub:
	ws_row    u16
	ws_col    u16
	ws_xpixel u16
	ws_ypixel u16
}

fn C.ioctl(fd int, request u64, arg voidptr) int

// get_terminal_size returns a number of columns and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return default_columns_size, default_rows_size
	}
	w := C.winsize{}
	unsafe { C.ioctl(1, u64(C.TIOCGWINSZ), &w) }
	return int(w.ws_col), int(w.ws_row)
}

// get_cursor_position returns a Coord containing the current cursor position
pub fn get_cursor_position() !Coord {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return Coord{0, 0}
	}

	mut old_state := termios.Termios{}
	if termios.tcgetattr(0, mut old_state) != 0 {
		return os.last_error()
	}
	defer {
		// restore the old terminal state:
		termios.tcsetattr(0, C.TCSANOW, mut old_state)
	}

	mut state := termios.Termios{}
	if termios.tcgetattr(0, mut state) != 0 {
		return os.last_error()
	}

	state.c_lflag &= termios.invert(u32(C.ICANON) | u32(C.ECHO))
	termios.tcsetattr(0, C.TCSANOW, mut state)

	print('\e[6n')
	flush_stdout()

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

// set_terminal_title change the terminal title (usually that is the containing terminal window title)
pub fn set_terminal_title(title string) bool {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return false
	}
	print('\033]0;')
	print(title)
	print('\007')
	flush_stdout()
	return true
}

// set_tab_title changes the terminal *tab title*, for terminal emulators like Konsole, that support several tabs
pub fn set_tab_title(title string) bool {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return false
	}
	print('\033]30;')
	print(title)
	print('\007')
	flush_stdout()
	return true
}

// clear clears current terminal screen.
pub fn clear() bool {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return false
	}
	print('\x1b[2J')
	print('\x1b[H')
	flush_stdout()
	return true
}

// supports_sixel returns `true` if the terminal supports Sixel graphics
//
// For more info on the sixel format:
// See https://en.wikipedia.org/wiki/Sixel
// See https://www.digiater.nl/openvms/decus/vax90b1/krypton-nasa/all-about-sixels.text
// For more info on terminal support:
// See https://www.arewesixelyet.com
pub fn supports_sixel() bool {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return false
	}
	if os.getenv('TERM') == 'yaft' {
		return true
	}

	mut old_state := termios.Termios{}
	if termios.tcgetattr(0, mut old_state) != 0 {
		return false
	}
	defer {
		// restore the old terminal state:
		termios.tcsetattr(0, C.TCSANOW, mut old_state)
	}

	mut state := termios.Termios{}
	if termios.tcgetattr(0, mut state) != 0 {
		return false
	}

	state.c_lflag &= termios.invert(u32(C.ICANON) | u32(C.ECHO))
	termios.tcsetattr(0, C.TCSANOW, mut state)

	// Send a "Query Device Code"
	print('\e[c')
	flush_stdout()

	// Terminal answers with a "Report Device Code" in the format `\e[<code><N>c`
	mut buf := []u8{cap: 20}

	for {
		w := unsafe { C.getchar() }
		if w < 0 {
			return false
		} else {
			buf << u8(w)
		}

		if w == `c` {
			break
		}
	}
	// returns `true` if the last digit before the ending `c` is a literal "4"
	return 52 in buf
}

// graphics_num_colors returns the number of color registers the terminal
// graphic attribute is set to use. This can be useful to know if the terminal
// is configured to support Sixel graphics.
//
// See "CSI ? Pi ; Pa ; Pv S" from https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
pub fn graphics_num_colors() u16 {
	if os.is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return 0
	}

	if os.getenv('TERM') == 'yaft' {
		return 256
	}

	mut old_state := termios.Termios{}
	if termios.tcgetattr(0, mut old_state) != 0 {
		return 0
	}
	defer {
		// restore the old terminal state:
		termios.tcsetattr(0, C.TCSANOW, mut old_state)
	}

	mut state := termios.Termios{}
	if termios.tcgetattr(0, mut state) != 0 {
		return 0
	}

	state.c_lflag &= termios.invert(u32(C.ICANON) | u32(C.ECHO))
	termios.tcsetattr(0, C.TCSANOW, mut state)

	// Send "CSI ? Pi ; Pa ; Pv S"
	print('\e[?1;1;0S')
	flush_stdout()

	mut sb := strings.new_builder(6)
	defer { unsafe { sb.free() } }
	for {
		w := unsafe { C.getchar() }
		if w < 0 {
			return 0
		} else if w == `S` {
			break
		} else if w == `;` {
			sb.clear()
		} else {
			sb.write_byte(u8(w))
		}
	}
	return sb.str().u16()
}
