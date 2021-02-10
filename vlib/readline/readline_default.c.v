// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// TODO Mac version needs to be implemented
// Will serve as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module readline

import os

#include <termios.h>
// Only use standard os.get_line
// Need implementation for readline capabilities
//
// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete input line as an UTF-8 encoded `ustring` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.
pub fn (mut r Readline) read_line_utf8(prompt string) ?ustring {
	r.current = ''.ustring()
	r.cursor = 0
	r.prompt = prompt
	r.search_index = 0
	if r.previous_lines.len <= 1 {
		r.previous_lines << ''.ustring()
		r.previous_lines << ''.ustring()
	} else {
		r.previous_lines[0] = ''.ustring()
	}
	print(r.prompt)
	line := os.get_raw_line()
	if line.len >= 0 {
		r.current = line.ustring()
	}
	r.previous_lines[0] = ''.ustring()
	r.search_index = 0
	if r.current.s == '' {
		return error('empty line')
	}
	return r.current
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
// (As opposed to `ustring` returned by `read_line_utf8`).
pub fn (mut r Readline) read_line(prompt string) ?string {
	s := r.read_line_utf8(prompt) ?
	return s.s
}

// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete input line as an UTF-8 encoded `ustring` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.
// NOTE that this version of `read_line_utf8` is a standalone function without
// persistent functionalities (e.g. history).
pub fn read_line_utf8(prompt string) ?ustring {
	mut r := Readline{}
	s := r.read_line_utf8(prompt) ?
	return s
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
// (As opposed to `ustring` as returned by `read_line_utf8`).
// NOTE that this version of `read_line` is a standalone function without
// persistent functionalities (e.g. history).
pub fn read_line(prompt string) ?string {
	mut r := Readline{}
	s := r.read_line(prompt) ?
	return s
}
