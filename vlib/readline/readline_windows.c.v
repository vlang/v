// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Windows currently supports basic line reading and raw-character input.
// The richer line-editing functionality is still implemented only on nix.
// Will serve as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module readline

import os

#include <conio.h>
#include <windows.h>

// needed for parity with readline_default.c.v
struct Termios {
}

fn C._getwch() i32

fn (mut r Readline) set_raw_mode(keep_processed_input bool) {
	if os.is_atty(0) <= 0 || os.getenv('TERM') == 'dumb' {
		r.is_tty = false
		r.is_raw = false
		return
	}
	stdin_handle := C.GetStdHandle(C.STD_INPUT_HANDLE)
	if stdin_handle == C.INVALID_HANDLE_VALUE || !C.GetConsoleMode(stdin_handle, &r.orig_stdin_mode) {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw_mode := r.orig_stdin_mode & (~u32(C.ENABLE_LINE_INPUT | C.ENABLE_ECHO_INPUT))
	if !keep_processed_input {
		raw_mode &= ~u32(C.ENABLE_PROCESSED_INPUT)
	}
	if !C.SetConsoleMode(stdin_handle, raw_mode) {
		r.is_tty = false
		r.is_raw = false
		return
	}
	r.is_tty = true
	r.is_raw = true
}

// enable_raw_mode enables the raw mode of the terminal.
// In raw mode all key presses are directly sent to the program and no interpretation is done.
pub fn (mut r Readline) enable_raw_mode() {
	r.set_raw_mode(false)
}

// enable_raw_mode_nosig enables the raw mode of the terminal while keeping console signal processing enabled.
pub fn (mut r Readline) enable_raw_mode_nosig() {
	r.set_raw_mode(true)
}

// disable_raw_mode disables the raw mode of the terminal.
pub fn (mut r Readline) disable_raw_mode() {
	if !r.is_raw {
		return
	}
	stdin_handle := C.GetStdHandle(C.STD_INPUT_HANDLE)
	if stdin_handle != C.INVALID_HANDLE_VALUE {
		C.SetConsoleMode(stdin_handle, r.orig_stdin_mode)
	}
	r.is_raw = false
}

// read_char reads a single character.
pub fn (r Readline) read_char() !int {
	ch := int(C._getwch())
	if ch < 0 {
		return error('failed to read character')
	}
	if ch !in [0, 0xe0] {
		return ch
	}
	extended := int(C._getwch())
	if extended < 0 {
		return error('failed to read character')
	}
	return int((u32(ch) << 16) | u32(extended))
}

// Only use standard os.get_line
// Need implementation for readline capabilities
//
// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete UTF-8 input line as an UTF-32 encoded `[]rune` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.
pub fn (mut r Readline) read_line_utf8(prompt string) ![]rune {
	r.current = []rune{}
	r.cursor = 0
	r.prompt = prompt
	r.search_index = 0
	if r.previous_lines.len <= 1 {
		r.previous_lines << []rune{}
		r.previous_lines << []rune{}
	} else {
		r.previous_lines[0] = []rune{}
	}
	print(r.prompt)
	flush_stdout()
	r.current = os.get_raw_line().runes()
	r.previous_lines[0] = []rune{}
	r.search_index = 0
	if r.current.len == 0 {
		return error('empty line')
	}
	return r.current
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
// (As opposed to `[]rune` returned by `read_line_utf8`).
pub fn (mut r Readline) read_line(prompt string) !string {
	s := r.read_line_utf8(prompt)!
	return s.string()
}

// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete UTF-8 input line as an UTF-32 encoded `[]rune` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.
// NOTE that this version of `read_line_utf8` is a standalone function without
// persistent functionalities (e.g. history).
pub fn read_line_utf8(prompt string) ![]rune {
	mut r := Readline{}
	s := r.read_line_utf8(prompt)!
	return s
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
// (As opposed to `[]rune` as returned by `read_line_utf8`).
// NOTE that this version of `read_line` is a standalone function without
// persistent functionalities (e.g. history).
pub fn read_line(prompt string) !string {
	mut r := Readline{}
	s := r.read_line(prompt)!
	return s
}
