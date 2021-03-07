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
pub fn get_terminal_size() (int, int) {
	if is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return default_columns_size, default_rows_size
	}
	w := C.winsize{}
	C.ioctl(1, u64(C.TIOCGWINSZ), &w)
	return int(w.ws_col), int(w.ws_row)
}

// get_cursor_position returns a Coord containing the current cursor position
pub fn get_cursor_position() Coord {
	if is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return Coord{
			x: 0
			y: 0
		}
	}
	// TODO: use termios.h, C.tcgetattr & C.tcsetattr directly,
	// instead of using `stty`
	oldsettings := os.exec('stty -g') or {
		os.Result{}
	}
	os.system('stty -echo -icanon time 0')
	print('\033[6n')
	mut ch := int(0)
	mut i := 0
	// ESC [ YYY `;` XXX `R`
	mut reading_x := false
	mut reading_y := false
	mut x := 0
	mut y := 0
	for {
		ch = C.getchar()
		b := byte(ch)
		i++
		if i >= 15 {
			panic('C.getchar() called too many times')
		}
		// state management:
		if b == `R` {
			break
		}
		if b == `[` {
			reading_y = true
			reading_x = false
			continue
		}
		if b == `;` {
			reading_y = false
			reading_x = true
			continue
		}
		// converting string vals to ints:
		if reading_x {
			x *= 10
			x += (b - byte(`0`))
		}
		if reading_y {
			y *= 10
			y += (b - byte(`0`))
		}
	}
	// restore the old terminal settings:
	os.system('stty $oldsettings.output')
	return Coord{
		x: x
		y: y
	}
}

// set_terminal_title change the terminal title
pub fn set_terminal_title(title string) bool {
	if is_atty(1) <= 0 || os.getenv('TERM') == 'dumb' {
		return true
	}
	print('\033]0;${title}\007')
	return true
}

// clear clears current terminal screen.
pub fn clear() {
	print('\x1b[2J')
	print('\x1b[H')
}
