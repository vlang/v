// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

[inline]
pub fn is_key_char(c byte) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) // || c == `_`  || c == `-` <- these are identified when tokenizing
}

// is_ascii_control_character returns true if `byte_char` is an ASCII control character.
[inline]
pub fn is_ascii_control_character(byte_char byte) bool {
	return (byte_char >= 0 && byte_char <= 0x1f) || byte_char == 0x7f
}

// is_illegal_ascii_control_character returns true if a `byte_char` ASCII control character
// is considered "illegal" in TOML .
[inline]
pub fn is_illegal_ascii_control_character(byte_char byte) bool {
	return byte_char != 0x09 && is_ascii_control_character(byte_char)
}

[if trace_toml ?]
pub fn printdbg(id string, message string) {
	eprintln(id + ' ' + message)
}

// parse_dotted_key converts `key` string to an array of strings.
// parse_dotted_key preserves strings delimited by both `"` and `'`.
pub fn parse_dotted_key(key string) ?[]string {
	mut out := []string{}
	mut buf := ''
	mut in_string := false
	mut delim := byte(` `)
	for ch in key {
		if ch in [`"`, `'`] {
			if !in_string {
				delim = ch
			}
			in_string = !in_string && ch == delim
			if !in_string {
				if buf != '' && buf != ' ' {
					out << buf
				}
				buf = ''
				delim = ` `
			}
			continue
		}
		buf += ch.ascii_str()
		if !in_string && ch == `.` {
			if buf != '' && buf != ' ' {
				out << buf[..buf.len - 1]
			}
			buf = ''
			continue
		}
	}
	if buf != '' && buf != ' ' {
		out << buf
	}
	if in_string {
		return error(@FN +
			': could not parse key, missing closing string delimiter `$delim.ascii_str()`')
	}
	return out
}
