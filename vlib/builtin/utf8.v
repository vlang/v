// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

const utf8_replacement_rune = rune(0xfffd)

// utf8_char_len returns the length in bytes of a UTF-8 encoded codepoint that starts with the byte `b`.
pub fn utf8_char_len(b u8) int {
	return int(((u32(0xe5000000) >> ((b >> 3) & 0x1e)) & 3) + 1)
}

// Convert utf32 to utf8
// utf32 == Codepoint
pub fn utf32_to_str(code u32) string {
	unsafe {
		mut buffer := malloc_noscan(5)
		res := utf32_to_str_no_malloc(code, mut buffer)
		if res.len == 0 {
			// the buffer was not used at all
			free(buffer)
		}
		return res
	}
}

@[manualfree; unsafe]
pub fn utf32_to_str_no_malloc(code u32, mut buf &u8) string {
	unsafe {
		len := utf32_decode_to_buffer(code, mut buf)
		if len == 0 {
			return ''
		}
		buf[len] = 0
		return tos(buf, len)
	}
}

@[manualfree; unsafe]
pub fn utf32_decode_to_buffer(code u32, mut buf &u8) int {
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

// Convert utf8 to utf32
// the original implementation did not check for
// valid utf8 in the string, and could result in
// values greater than the utf32 spec
// it has been replaced by `utf8_to_utf32` which
// has an option return type.
//
// this function is left for backward compatibility
// it is used in vlib/builtin/string.v,
// and also in vlib/v/gen/c/cgen.v
pub fn (_rune string) utf32_code() int {
	if _rune.len > 4 {
		return 0
	}
	return int(impl_utf8_to_utf32(_rune.str, _rune.len))
}

// convert array of utf8 bytes to single utf32 value
// will error if more than 4 bytes are submitted
pub fn (_bytes []u8) utf8_to_utf32() !rune {
	if _bytes.len > 4 {
		return error('attempted to decode too many bytes, utf-8 is limited to four bytes maximum')
	}
	return impl_utf8_to_utf32(_bytes.data, _bytes.len)
}

@[inline]
fn utf8_is_continuation(b u8) bool {
	return (b & 0xc0) == 0x80
}

@[direct_array_access]
fn utf8_decode_rune(_bytes &u8, available_len int) (rune, int) {
	if available_len <= 0 {
		return 0, 0
	}
	b0 := unsafe { _bytes[0] }
	if b0 < 0x80 {
		return rune(b0), 1
	}
	if b0 < 0xc2 {
		return utf8_replacement_rune, 1
	}
	char_len := if b0 < 0xe0 {
		2
	} else if b0 < 0xf0 {
		3
	} else if b0 < 0xf5 {
		4
	} else {
		return utf8_replacement_rune, 1
	}
	if available_len < char_len {
		return utf8_replacement_rune, 1
	}
	b1 := unsafe { _bytes[1] }
	if !utf8_is_continuation(b1) {
		return utf8_replacement_rune, 1
	}
	if char_len == 2 {
		return ((rune(b0) & 0x1f) << 6) | (rune(b1) & 0x3f), 2
	}
	if b0 == 0xe0 && b1 < 0xa0 {
		return utf8_replacement_rune, 1
	}
	if b0 == 0xed && b1 >= 0xa0 {
		return utf8_replacement_rune, 1
	}
	b2 := unsafe { _bytes[2] }
	if !utf8_is_continuation(b2) {
		return utf8_replacement_rune, 1
	}
	if char_len == 3 {
		return ((rune(b0) & 0x0f) << 12) | ((rune(b1) & 0x3f) << 6) | (rune(b2) & 0x3f), 3
	}
	if b0 == 0xf0 && b1 < 0x90 {
		return utf8_replacement_rune, 1
	}
	if b0 == 0xf4 && b1 > 0x8f {
		return utf8_replacement_rune, 1
	}
	b3 := unsafe { _bytes[3] }
	if !utf8_is_continuation(b3) {
		return utf8_replacement_rune, 1
	}
	return ((rune(b0) & 0x07) << 18) | ((rune(b1) & 0x3f) << 12) | ((rune(b2) & 0x3f) << 6) | (rune(b3) & 0x3f), 4
}

@[direct_array_access]
fn impl_utf8_to_utf32(_bytes &u8, _bytes_len int) rune {
	if _bytes_len == 0 || _bytes_len > 4 {
		return 0
	}
	r, len := utf8_decode_rune(_bytes, _bytes_len)
	if len != _bytes_len {
		return utf8_replacement_rune
	}
	return r
}

// Calculate string length for formatting, i.e. number of "characters"
// This is simplified implementation. if you need specification compliant width,
// use utf8.east_asian.display_width.
pub fn utf8_str_visible_length(s string) int {
	return utf8_grapheme_visible_length(s)
}

// string_to_ansi_not_null_terminated returns an ANSI version of the string `_str`.
// NOTE: This is most useful for converting a vstring to an ANSI string under Windows.
// NOTE: The ANSI string return is not null-terminated, then you can use `os.write_file_array` write an ANSI file.
pub fn string_to_ansi_not_null_terminated(_str string) []u8 {
	wstr := _str.to_wide()
	mut ansi := wide_to_ansi(wstr)
	if ansi.len > 0 {
		unsafe { ansi.len-- } // remove tailing zero
	}
	return ansi
}
