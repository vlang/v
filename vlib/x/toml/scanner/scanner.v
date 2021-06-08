// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import os
import math.mathutil
import x.toml.input
import x.toml.token

// Scanner contains the necessary fields for the state of the scan process.
// the task the scanner does is also refered to as "lexing" or "tokenizing".
// The Scanner methods are based on much of the work in `vlib/strings/textscanner`.
pub struct Scanner {
pub:
	config Config
	text   string // the input TOML text
mut:
	col     int // current column number (x coordinate)
	line_nr int // current line number (y coordinate)
	pos     int // current flat/index position in the `text` field
	mode    Mode // sub-mode of the scanner
}

enum Mode {
	normal
	inside_string
}

// Config is used to configure a Scanner instance.
// Only one of the fields `text` and `file_path` is allowed to be set at time of configuration.
pub struct Config {
pub:
	input input.Config
	tokenize_formating bool // if true, generate tokens for `\n`, ` `, `\t`, `\r` etc.
}

// new_scanner returns a new heap allocated `Scanner` instance.
pub fn new_scanner(config Config) &Scanner {
	config.input.validate()
	mut text := config.input.text
	file_path := config.input.file_path
	if os.is_file(file_path) {
		text = os.read_file(file_path) or {
			panic(@MOD + '.' + @FN + ' Could not read "$file_path": "$err.msg"')
		}
	}
	mut s := &Scanner{
		config: config
		text: text
	}
	return s
}

[direct_array_access]
pub fn (mut s Scanner) scan() token.Token {
	for {
		c := s.next()
		if c == -1 {
			return s.new_token(.eof, '', 1)
		}
		match rune(c) {
			`#` {
				start := s.pos + 1
				s.ignore_line()
				hash := s.text[start..s.pos]
				return s.new_token(.hash, hash, hash.len + 2)
			}
			else {
				panic(@MOD + '.' + @FN + ' Could not scan character code $c at $s.pos ($s.line_nr,$s.col) "${s.text[s.pos]}"')
			}
		}
	}
}

// free frees all allocated resources
[unsafe]
pub fn (mut s Scanner) free() {
	unsafe {
		s.text.free()
	}
}

// remaining returns how many characters remain in the text input
[inline]
pub fn (s &Scanner) remaining() int {
	return s.text.len - s.pos
}

// next returns the next character code from the input text.
// next returns `-1` if it can't reach the next character.
[direct_array_access; inline]
pub fn (mut s Scanner) next() int {
	if s.pos < s.text.len {
		opos := s.pos
		s.pos++
		c := s.text[opos]
		if c == `\n` {
			s.col++
		}
		return c
	}
	return -1
}

// skip skips one character ahead.
[inline]
pub fn (mut s Scanner) skip() {
	if s.pos + 1 < s.text.len {
		s.pos++
	}
}

// skip_n skips ahead `n` characters.
// If the skip goes out of bounds from the length of `Scanner.text`,
// the scanner position will be sat to the last character possible.
[inline]
pub fn (mut s Scanner) skip_n(n int) {
	s.pos += n
	if s.pos > s.text.len {
		s.pos = s.text.len
	}
}

// peek returns the *next* character code from the input text.
// peek returns `-1` if it can't peek the next character.
// unlike `next()`, `peek()` does not change the state of the scanner.
[direct_array_access; inline]
pub fn (s &Scanner) peek() int {
	if s.pos < s.text.len {
		return s.text[s.pos]
	}
	return -1
}

// peek_n returns the character code from the input text at position + `n`.
// peek_n returns `-1` if it can't peek `n` characters ahead.
[direct_array_access; inline]
pub fn (s &Scanner) peek_n(n int) int {
	if s.pos + n < s.text.len {
		return s.text[s.pos + n]
	}
	return -1
}

// back goes back 1 character from the current scanner position.
[inline]
pub fn (mut s Scanner) back() {
	if s.pos > 0 {
		s.pos--
	}
}

// back_n goes back `n` characters from the current scanner position.
pub fn (mut s Scanner) back_n(n int) {
	s.pos -= n
	if s.pos < 0 {
		s.pos = 0
	}
	if s.pos > s.text.len {
		s.pos = s.text.len
	}
}

// reset resets the internal state of the scanner.
pub fn (mut s Scanner) reset() {
	s.pos = 0
}

// new_token returns a new `token.Token`.
[inline]
fn (mut s Scanner) new_token(kind token.Kind, lit string, len int) token.Token {
	line_offset := 1
	//println('new_token($lit)')
	return token.Token{
		kind: kind
		lit: lit
		col: mathutil.max(1, s.col - len + 1)
		line_nr: s.line_nr + line_offset
		pos: s.pos - len + 1
		len: len
	}
}

[inline]
fn (mut s Scanner) ignore_line() {
	s.eat_to_end_of_line()
	s.inc_line_number()
}

[inline]
fn (mut s Scanner) inc_line_number() {
	s.line_nr++
}

[direct_array_access; inline]
fn (mut s Scanner) eat_to_end_of_line() {
	for c := s.next(); c != -1 && c != `\n`; c = s.next() {
		//println('skipping ${byte(c).ascii_str()}')
		continue
	}
	s.next()
}
