module term

import os

pub struct Coord16 {
pub:
	x i16
	y i16
}

struct SmallRect {
	left   i16
	top    i16
	right  i16
	bottom i16
}

// win: CONSOLE_SCREEN_BUFFER_INFO
// https://docs.microsoft.com/en-us/windows/console/console-screen-buffer-info-str
struct ConsoleScreenBufferInfo {
	dw_size                Coord16
	dw_cursor_position     Coord16
	w_attributes           u16
	sr_window              SmallRect
	dw_maximum_window_size Coord16
}

// ref - https://docs.microsoft.com/en-us/windows/console/getconsolescreenbufferinfo
fn C.GetConsoleScreenBufferInfo(handle os.HANDLE, info &ConsoleScreenBufferInfo) bool

// ref - https://docs.microsoft.com/en-us/windows/console/setconsoletitle
fn C.SetConsoleTitle(title &u16) bool

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

// get_cursor_position returns a Coord containing the current cursor position
pub fn get_cursor_position() Coord {
	mut res := Coord{}
	if is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := ConsoleScreenBufferInfo{}
		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			res.x = info.dw_cursor_position.x
			res.y = info.dw_cursor_position.y
		}
	}
	return res
}

// set_terminal_title change the terminal title
pub fn set_terminal_title(title string) bool {
	title_change := C.SetConsoleTitle(title.to_wide())
	return title_change
}
