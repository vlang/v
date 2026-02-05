// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM-specific readline implementation
// Provides stub implementations for terminal raw mode and character reading
// since direct terminal control is not available on the BEAM runtime.
//
module readline

import term.termios
import term
import os

// enable_raw_mode enables the raw mode of the terminal.
// In raw mode all key presses are directly sent to the program and no interpretation is done.
// On BEAM: This is a stub that sets flags but actual raw mode depends on the BEAM runtime.
pub fn (mut r Readline) enable_raw_mode() {
	if termios.tcgetattr(0, mut r.orig_termios) != 0 {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw := r.orig_termios
	// Set raw mode flags (these are stored but may not take effect on BEAM)
	raw.c_iflag &= termios.invert(termios.brkint | termios.icrnl | termios.inpck | termios.istrip | termios.ixon)
	raw.c_cflag |= termios.flag(int(termios.cs8))
	raw.c_lflag &= termios.invert(termios.echo | termios.icanon | termios.iexten | termios.isig)
	raw.c_cc[termios.vmin] = u8(1)
	raw.c_cc[termios.vtime] = u8(0)
	termios.tcsetattr(0, termios.tcsadrain, mut raw)
	r.is_raw = true
	r.is_tty = true
}

// enable_raw_mode_nosig enables the raw mode of the terminal.
// In raw mode all key presses are directly sent to the program and no interpretation is done.
// This version does not catch the SIGUSER (CTRL + C) signal.
// On BEAM: This is a stub that sets flags but actual raw mode depends on the BEAM runtime.
pub fn (mut r Readline) enable_raw_mode_nosig() {
	if termios.tcgetattr(0, mut r.orig_termios) != 0 {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw := r.orig_termios
	// Set raw mode flags without ISIG (these are stored but may not take effect on BEAM)
	raw.c_iflag &= termios.invert(termios.brkint | termios.icrnl | termios.inpck | termios.istrip | termios.ixon)
	raw.c_cflag |= termios.flag(int(termios.cs8))
	raw.c_lflag &= termios.invert(termios.echo | termios.icanon | termios.iexten)
	raw.c_cc[termios.vmin] = u8(1)
	raw.c_cc[termios.vtime] = u8(0)
	termios.tcsetattr(0, termios.tcsadrain, mut raw)
	r.is_raw = true
	r.is_tty = true
}

// disable_raw_mode disables the raw mode of the terminal.
// On BEAM: This is a stub that attempts to restore original termios settings.
pub fn (mut r Readline) disable_raw_mode() {
	if r.is_raw {
		termios.tcsetattr(0, termios.tcsadrain, mut r.orig_termios)
		r.is_raw = false
	}
}

// read_char reads a single character.
// On BEAM: Uses term.utf8_getchar which may have limited functionality.
pub fn (r Readline) read_char() !int {
	return int(term.utf8_getchar() or { return err })
}

// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// On BEAM: Falls back to os.get_raw_line for input.
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
	// On BEAM, use simple line input since raw mode may not work
	line := os.get_raw_line()
	if line.len >= 0 {
		r.current = line.runes()
	}
	r.previous_lines[0] = []rune{}
	r.search_index = 0
	if r.current.len == 0 {
		return error('empty line')
	}
	return r.current
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
pub fn (mut r Readline) read_line(prompt string) !string {
	s := r.read_line_utf8(prompt)!
	return s.string()
}

// read_line_utf8 standalone function without persistent functionalities.
pub fn read_line_utf8(prompt string) ![]rune {
	mut r := Readline{}
	s := r.read_line_utf8(prompt)!
	return s
}

// read_line standalone function without persistent functionalities.
pub fn read_line(prompt string) !string {
	mut r := Readline{}
	s := r.read_line(prompt)!
	return s
}
