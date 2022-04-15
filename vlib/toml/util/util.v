// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

[inline]
pub fn is_key_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) // || c == `_`  || c == `-` <- these are identified when tokenizing
}

// is_ascii_control_character returns true if `byte_char` is an ASCII control character.
[inline]
pub fn is_ascii_control_character(byte_char u8) bool {
	return (byte_char >= 0 && byte_char <= 0x1f) || byte_char == 0x7f
}

// is_illegal_ascii_control_character returns true if a `byte_char` ASCII control character
// is considered "illegal" in TOML .
[inline]
pub fn is_illegal_ascii_control_character(byte_char u8) bool {
	return byte_char != 0x09 && is_ascii_control_character(byte_char)
}

[if trace_toml ?]
pub fn printdbg(id string, message string) {
	eprintln(id + ' ' + message)
}
