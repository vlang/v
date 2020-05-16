module term

import os

struct Coord {
	x i16
	y i16
}

struct SmallRect {
	left   i16
	top    i16
	right  i16
	bottom i16
}

struct ConsoleScreenBufferInfo {
	dw_size                Coord
	dw_cursor_position     Coord
	w_attributes           u16
	sr_window              SmallRect
	dw_maximum_window_size Coord
}

fn C.GetConsoleScreenBufferInfo(handle os.HANDLE, info &ConsoleScreenBufferInfo) bool

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := ConsoleScreenBufferInfo{}
		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			columns := int(info.sr_window.right - info.sr_window.left + 1)
			rows := int(info.sr_window.bottom - info.sr_window.top + 1)
			return columns, rows
		}
	}
	return default_columns_size, default_rows_size
}
