// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

pub fn utf8_char_len(b u8) int {
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
pub fn utf32_to_str_no_malloc(code u32, buf &u8) string {
	unsafe {
		len := utf32_decode_to_buffer(code, buf)
		if len == 0 {
			return ''
		}
		buf[len] = 0
		return tos(buf, len)
	}
}

[manualfree; unsafe]
pub fn utf32_decode_to_buffer(code u32, buf &u8) int {
	unsafe {
		icode := int(code) // Prevents doing casts everywhere
		mut buffer := &u8(buf)
		if icode <= 127 {
			// 0x7F
			buffer[0] = u8(icode)
			return 1
		} else if icode <= 2047 {
			// 0x7FF
			buffer[0] = 192 | u8(icode >> 6) // 0xC0 - 110xxxxx
			buffer[1] = 128 | u8(icode & 63) // 0x80 - 0x3F - 10xxxxxx
			return 2
		} else if icode <= 65535 {
			// 0xFFFF
			buffer[0] = 224 | u8(icode >> 12) // 0xE0 - 1110xxxx
			buffer[1] = 128 | (u8(icode >> 6) & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[2] = 128 | u8(icode & 63) // 0x80 - 0x3F - 10xxxxxx
			return 3
		}
		// 0x10FFFF
		else if icode <= 1114111 {
			buffer[0] = 240 | u8(icode >> 18) // 0xF0 - 11110xxx
			buffer[1] = 128 | (u8(icode >> 12) & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[2] = 128 | (u8(icode >> 6) & 63) // 0x80 - 0x3F - 10xxxxxx
			buffer[3] = 128 | u8(icode & 63) // 0x80 - 0x3F - 10xxxxxx
			return 4
		}
	}
	return 0
}

// utf8_str_len returns the number of runes contained in the string.
[deprecated: 'use `string.len_utf8()` instead']
[deprecated_after: '2022-05-28']
pub fn utf8_str_len(s string) int {
	mut l := 0
	mut i := 0
	for i < s.len {
		l++
		i += ((0xe5000000 >> ((unsafe { s.str[i] } >> 3) & 0x1e)) & 3) + 1
	}
	return l
}

// Convert utf8 to utf32
// the original implementation did not check for
// valid utf8 in the string, and could result in
// values greater than the utf32 spec
// it has been replaced by `utf8_to_utf32` which
// has an optional return type.
//
// this function is left for backward compatibility
// it is used in vlib/builtin/string.v,
// and also in vlib/v/gen/c/cgen.v
pub fn (_rune string) utf32_code() int {
	return int(_rune.bytes().utf8_to_utf32() or {
		// error('more than one utf-8 rune found in this string')
		rune(0)
	})
}

// convert array of utf8 bytes to single utf32 value
// will error if more than 4 bytes are submitted
pub fn (_bytes []u8) utf8_to_utf32() ?rune {
	if _bytes.len == 0 {
		return 0
	}
	// return ASCII unchanged
	if _bytes.len == 1 {
		return rune(_bytes[0])
	}
	if _bytes.len > 4 {
		return error('attempted to decode too many bytes, utf-8 is limited to four bytes maximum')
	}

	mut b := u8(int(_bytes[0]))

	b = b << _bytes.len
	mut res := rune(b)
	mut shift := 6 - _bytes.len
	for i := 1; i < _bytes.len; i++ {
		c := rune(_bytes[i])
		res = rune(res) << shift
		res |= c & 63 // 0x3f
		shift = 6
	}
	return res
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
