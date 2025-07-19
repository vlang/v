// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module term

// Sources for ANSI Control Sequences
// https://github.com/RajeshPatkarInstitute/Panim
// https://www.gnu.org/software/screen/manual/html_node/Control-Sequences.html
// https://en.wikipedia.org/wiki/ANSI_escape_code
// Support for Windows
// https://en.wikipedia.org/wiki/ANSI.SYS
// #include <windows.h>
// C.SetConsoleMode(C.ENABLE_VIRTUAL_TERMINAL_INPUT)
// Setting cursor to the given position
// x is the x coordinate
// y is the y coordinate
pub fn set_cursor_position(c Coord) {
	print('\x1b[${c.y};${c.x}' + 'H')
	flush_stdout()
}

// move the cursor relative to its current position.
// n is number of cells.
// direction: A is up / North
// direction: B is down / South
// direction: C is forward / East
// direction: D is backward / West
pub fn move(n int, direction string) {
	print('\x1b[${n}${direction}')
	flush_stdout()
}

// cursor_up moves the cursor up `n` lines.
pub fn cursor_up(n int) {
	move(n, 'A')
}

// cursor_down moves the cursor down `n` lines.
pub fn cursor_down(n int) {
	move(n, 'B')
}

// cursor_forward moves the cursor forward `n` character positions.
pub fn cursor_forward(n int) {
	move(n, 'C')
}

// cursor_back moves the cursor back `n` characters.
pub fn cursor_back(n int) {
	move(n, 'D')
}

// erase_display erases display characters based on the given parameter, `t`.
// `t` can be of the following values in string:
// `0`: current cursor position to end of the terminal window.
// `1`: current cursor position to beginning of the terminal
// window.
// `2`: clears the entire terminal window.
// `3`: clears the entire terminal window and also deletes the scrollback buffer.
pub fn erase_display(t string) {
	print('\x1b[' + t + 'J')
	flush_stdout()
}

// erase_to_end erases from the cursor to the end of the terminal window.
pub fn erase_toend() {
	erase_display('0')
}

// erase_tobeg erases from the cursor to the beginning of the terminal window.
pub fn erase_tobeg() {
	erase_display('1')
}

// erase_clear clears the entire terminal window and returns the cursor to the top left corner.
pub fn erase_clear() {
	print('\033[H\033[J')
	flush_stdout()
}

// erase_del_clear erases saved lines.
pub fn erase_del_clear() {
	erase_display('3')
}

// erase_line erases the current line based on the given parameter, `t`.
// `t` can be of the following values in string:
// `0`: current cursor position to the end of the line.
// `1`: current cursor position to the beginning of the line.
// `2`: clears the entire line.
// Note: Cursor position does not change.
pub fn erase_line(t string) {
	print('\x1b[' + t + 'K')
	flush_stdout()
}

// erase_line_toend erases from the cursor position to the end of the line.
pub fn erase_line_toend() {
	erase_line('0')
}

// erase_line_tobeg erases from the start of the line to the cursor position.
pub fn erase_line_tobeg() {
	erase_line('1')
}

// erase_line_clear erases the entire line.
pub fn erase_line_clear() {
	erase_line('2')
}

// show_cursor makes the cursor appear if it was not visible.
pub fn show_cursor() {
	print('\x1b[?25h')
	flush_stdout()
}

// Will make cursor invisible
pub fn hide_cursor() {
	print('\x1b[?25l')
	flush_stdout()
}

// clear_previous_line - useful for progressbars.
// It moves the cursor to start of line, then 1 line above,
// then erases the line. In effect the next println will overwrite
// the previous content.
pub fn clear_previous_line() {
	print('\r\x1b[1A\x1b[2K')
	flush_stdout()
}
