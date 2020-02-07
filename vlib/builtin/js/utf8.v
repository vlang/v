// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn utf8_char_len(b byte) int {
	return (( 0xe5000000 >> (( b >> 3 ) & 0x1e )) & 3 ) + 1
}

// Convert utf32 to utf8
// utf32 == Codepoint
pub fn utf32_to_str(code u32) string {
	return ''
}

// TODO copypasta
pub fn utf32_to_str_no_malloc(code u32, buf voidptr) string {
	return ''
}

// Convert utf8 to utf32
pub fn (_rune string) utf32_code() int {
	return 0
}


