module term

import os

struct Coord {
	x i16
	y i16
}

struct SmallRect {
	Left   i16
	Top    i16
	Right  i16
	Bottom i16
}

struct ConsoleScreenBufferInfo {
	dwSize              Coord
	dwCursorPosition    Coord
	wAttributes         u16
	srWindow            SmallRect
	dwMaximumWindowSize Coord
}

fn C.GetConsoleScreenBufferInfo(handle os.HANDLE, info &ConsoleScreenBufferInfo) bool

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := ConsoleScreenBufferInfo{}

		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			columns := int(info.srWindow.Right - info.srWindow.Left + 1)
			rows := int(info.srWindow.Bottom - info.srWindow.Top + 1)
			return columns, rows
		}
	}

	return default_columns_size, default_rows_size
}
