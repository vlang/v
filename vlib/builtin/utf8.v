// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

pub fn utf8_char_len(b byte) int {
	return ((0xe5000000 >> ((b >> 3) & 0x1e)) & 3) + 1
}

// Convert utf32 to utf8
// utf32 == Codepoint
pub fn utf32_to_str(code u32) string {
	unsafe {
		mut buffer := malloc_noscan(5)
		res := utf32_to_str_no_malloc(code, buffer)
		if res.len == 0 {
			// the buffer was not used at all
			free(buffer)
		}
		return res
	}
}

[manualfree; unsafe]
pub fn utf32_to_str_no_malloc(code u32, buf &byte) string {
	unsafe {
		icode := int(code) // Prevents doing casts everywhere
		mut buffer := &byte(buf)
		if icode <= 127 {
			// 0x7F
			buffer[0] = byte(icode)
			buffer[1] = 0
			return tos(buffer, 1)
		} else if icode <= 2047 {
			// 0x7FF
			buffer[0] = 192 | byte(icode >> 6) // 0xC0 - 110xxxxx
			buffer[1] = 128 | byte(icode & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[2] = 0
			return tos(buffer, 2)
		} else if icode <= 65535 {
			// 0xFFFF
			buffer[0] = 224 | byte(icode >> 12) // 0xE0 - 1110xxxx
			buffer[1] = 128 | (byte(icode >> 6) & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[2] = 128 | byte(icode & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[3] = 0
			return tos(buffer, 3)
		}
		// 0x10FFFF
		else if icode <= 1114111 {
			buffer[0] = 240 | byte(icode >> 18) // 0xF0 - 11110xxx
			buffer[1] = 128 | (byte(icode >> 12) & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[2] = 128 | (byte(icode >> 6) & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[3] = 128 | byte(icode & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[4] = 0
			return tos(buffer, 4)
		}
	}
	return ''
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
	b = b << _rune.len
	mut res := u32(b)
	mut shift := 6 - _rune.len
	for i := 1; i < _rune.len; i++ {
		c := u32(_rune[i])
		res = u32(res) << shift
		res |= c & 63 // 0x3f
		shift = 6
	}
	return int(res)
}

// Calculate length to read from the first byte
fn utf8_len(c byte) int {
	mut b := 0
	mut x := c
	if (x & 240) != 0 {
		// 0xF0
		x >>= 4
	} else {
		b += 4
	}
	if (x & 12) != 0 {
		// 0x0C
		x >>= 2
	} else {
		b += 2
	}
	if (x & 2) == 0 {
		// 0x02
		b++
	}
	return b
}

// Calculate string length for in number of codepoints
pub fn utf8_str_len(s string) int {
	mut l := 0
	mut i := 0
	for i < s.len {
		l++
		i += ((0xe5000000 >> ((unsafe { s.str[i] } >> 3) & 0x1e)) & 3) + 1
	}
	return l
}

// Calculate string length for formatting, i.e. number of "characters"
// This is simplified implementation. if you need specification compliant width,
// use utf8.east_asian.display_width.
pub fn utf8_str_visible_length(s string) int {
	mut l := 0
	mut ul := 1
	for i := 0; i < s.len; i += ul {
		c := unsafe { s.str[i] }
		ul = ((0xe5000000 >> ((unsafe { s.str[i] } >> 3) & 0x1e)) & 3) + 1
		if i + ul > s.len { // incomplete UTF-8 sequence
			return l
		}
		l++
		// avoid the match if not needed
		if ul == 1 {
			continue
		}
		// recognize combining characters and wide characters
		match ul {
			2 {
				r := u64((u16(c) << 8) | unsafe { s.str[i + 1] })
				if r >= 0xcc80 && r < 0xcdb0 {
					// diacritical marks
					l--
				}
			}
			3 {
				r := u64((u32(c) << 16) | unsafe { (u32(s.str[i + 1]) << 8) | s.str[i + 2] })
				// diacritical marks extended
				// diacritical marks supplement
				// diacritical marks for symbols
				if (r >= 0xe1aab0 && r <= 0xe1ac7f)
					|| (r >= 0xe1b780 && r <= 0xe1b87f)
					|| (r >= 0xe28390 && r <= 0xe2847f)
					|| (r >= 0xefb8a0 && r <= 0xefb8af) {
					// diacritical marks
					l--
				}
				// Hangru
				// CJK Unified Ideographics
				// Hangru
				// CJK
				else if (r >= 0xe18480 && r <= 0xe1859f)
					|| (r >= 0xe2ba80 && r <= 0xe2bf95)
					|| (r >= 0xe38080 && r <= 0xe4b77f)
					|| (r >= 0xe4b880 && r <= 0xea807f)
					|| (r >= 0xeaa5a0 && r <= 0xeaa79f)
					|| (r >= 0xeab080 && r <= 0xed9eaf)
					|| (r >= 0xefa480 && r <= 0xefac7f)
					|| (r >= 0xefb8b8 && r <= 0xefb9af) {
					// half marks
					l++
				}
			}
			4 {
				r := u64((u32(c) << 24) | unsafe {
					(u32(s.str[i + 1]) << 16) | (u32(s.str[i + 2]) << 8) | s.str[i + 3]
				})
				// Enclosed Ideographic Supplement
				// Emoji
				// CJK Unified Ideographs Extension B-G
				if (r >= 0x0f9f8880 && r <= 0xf09f8a8f)
					|| (r >= 0xf09f8c80 && r <= 0xf09f9c90)
					|| (r >= 0xf09fa490 && r <= 0xf09fa7af)
					|| (r >= 0xf0a08080 && r <= 0xf180807f) {
					l++
				}
			}
			else {}
		}
	}
	return l
}
