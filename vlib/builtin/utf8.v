// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

pub fn utf8_char_len(b byte) int {
	return ((0xe5000000>>((b>>3) & 0x1e)) & 3) + 1
}

// Convert utf32 to utf8
// utf32 == Codepoint
pub fn utf32_to_str(code u32) string {
	mut buffer := malloc(5)
	return utf32_to_str_no_malloc(code, buffer)
}

pub fn utf32_to_str_no_malloc(code u32, buf voidptr) string {
	icode := int(code) // Prevents doing casts everywhere
	mut res := ''
	unsafe {
		mut buffer := byteptr(buf)
		if icode <= 127 { /* 0x7F */
			buffer[0] = byte(icode)
			res = tos(buffer, 1)
		}
		else if icode <= 2047 { /* 0x7FF */
			buffer[0] = 192 | byte(icode>>6)  /* 0xC0 - 110xxxxx */
			buffer[1] = 128 | byte(icode & 63) /* 0x80 - 0x3F - 10xxxxxx */
			res = tos(buffer, 2)
		}
		else if icode <= 65535 { /* 0xFFFF */
			buffer[0] = 224 | byte(icode>>12)/* 0xE0 - 1110xxxx */
			buffer[1] = 128 | (byte(icode>>6) & 63) /* 0x80 - 0x3F - 10xxxxxx */
			buffer[2] = 128 | byte(icode & 63) /* 0x80 - 0x3F - 10xxxxxx */
			res = tos(buffer, 3)
		}
		else if icode <= 1114111/* 0x10FFFF */ {
			buffer[0] = 240 | byte(icode>>18)  /* 0xF0 - 11110xxx */
			buffer[1] = 128 | (byte(icode>>12) & 63) /* 0x80 - 0x3F - 10xxxxxx */
			buffer[2] = 128 | (byte(icode>>6) & 63) /* 0x80 - 0x3F - 10xxxxxx */
			buffer[3] = 128 | byte(icode & 63) /* 0x80 - 0x3F - 10xxxxxx */
			res = tos(buffer, 4)
		}
	}
	res.is_lit = 1 // let autofree know this string doesn't have to be freed
	return res
}

// Convert utf8 to utf32
pub fn (_rune string) utf32_code() int {
	if _rune.len == 0 {
		return 0
	}
	// save ASC symbol as is
	if _rune.len == 1 {
		return int(_rune[0])
	}
	mut b := byte(int(_rune[0]))
	// TODO should be
	// res := int( rune[0] << rune.len)
	b = b<<_rune.len
	mut res := int(b)
	mut shift := 6 - _rune.len
	for i := 1; i < _rune.len; i++ {
		c := int(_rune[i])
		res = res<<shift
		res |= c & 63 // 0x3f
		shift = 6
	}
	return res
}

// Calculate length to read from the first byte
fn utf8_len(c byte) int {
	mut b := 0
	mut x := c
	if (x & 240) != 0 {
		// 0xF0
		x >>= 4
	}
	else {
		b += 4
	}
	if (x & 12) != 0 {
		// 0x0C
		x >>= 2
	}
	else {
		b += 2
	}
	if (x & 2) == 0 {
		// 0x02
		b++
	}
	return b
}

// Calculate string length for in number of codepoints
fn utf8_str_len(s string) int {
	mut l := 0
	for i := 0; i < s.len; i++ {
		l++
		c := unsafe {s.str[i]}
		if (c & (1 << 7)) != 0 {
			for t := byte(1 << 6); (c & t) != 0; t >>= 1 {
				i++
			}
		}
	}
	return l
}

// Calculate string length for formatting, i.e. number of "characters"
fn utf8_str_visible_length(s string) int {
	mut l := 0
	mut ul := 1
	for i := 0; i < s.len; i+=ul {
		ul = 1
		c := unsafe {s.str[i]}
		if (c & (1 << 7)) != 0 {
			for t := byte(1 << 6); (c & t) != 0; t >>= 1 {
				ul++
			}
		}
		if i + ul > s.len { // incomplete UTF-8 sequence
			return l
		}
		l++
		// recognize combining characters
		if c == 0xcc || c == 0xcd {
			r := (u16(c) << 8) | unsafe {s.str[i+1]}
			if r >= 0xcc80 && r < 0xcdb0 { // diacritical marks
				l--
			}
		} else if c == 0xe1 || c == 0xe2 || c == 0xef {
			r := (u32(c) << 16) | unsafe {(u32(s.str[i+1]) << 8) | s.str[i+2]}
			// diacritical marks extended 0xe1aab0 - 0xe1ac80
			// diacritical marks supplement 0xe1b780 - 0xe1b880
			// diacritical marks for symbols 0xe28390 - 0xe28480
			// half marks 0xefb8a0 - 0xefb8b0
			if (r >= 0xe1aab0 && r < 0xe1ac80)
			|| (r >= 0xe1b780 && r < 0xe1b880)
			|| (r >= 0xe28390 && r < 0xe28480)
			|| (r >= 0xefb8a0 && r < 0xefb8b0) {
				l--
			}
		}
	}
	return l
}
