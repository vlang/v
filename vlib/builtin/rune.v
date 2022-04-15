// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

import strings

// This was never working correctly, the issue is now
// fixed however the type checks in checker need to be
// updated. if you uncomment it you will see the issue
// type rune = int

pub fn (c rune) str() string {
	return utf32_to_str(u32(c))
	/*
	unsafe {
		fst_byte := int(c)>>8 * 3 & 0xff
		len := utf8_char_len(u8(fst_byte))
		println('len=$len')
		mut str := string{
			len: len
			str: malloc_noscan(len + 1)
		}
		for i in 0..len {
			str.str[i] = u8(int(c)>>8 * (3 - i) & 0xff)
		}
		str.str[len] = `\0`
		println(str)
		return str
	}
	*/
}

// string converts a rune array to a string
[manualfree]
pub fn (ra []rune) string() string {
	mut sb := strings.new_builder(ra.len)
	sb.write_runes(ra)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// repeat returns a new string with `count` number of copies of the rune it was called on.
pub fn (c rune) repeat(count int) string {
	if count < 0 {
		panic('rune.repeat: count is negative: $count')
	} else if count == 0 {
		return ''
	} else if count == 1 {
		return c.str()
	}
	mut buffer := [5]u8{}
	res := unsafe { utf32_to_str_no_malloc(u32(c), &buffer[0]) }
	return res.repeat(count)
}

[manualfree]
pub fn (c rune) bytes() []u8 {
	mut res := []u8{cap: 5}
	res.len = unsafe { utf32_decode_to_buffer(u32(c), &u8(res.data)) }
	return res
}

// length_in_bytes returns the number of bytes needed to store the code point.
// Returns -1 if the data is not a valid code point.
pub fn (c rune) length_in_bytes() int {
	code := u32(c)
	if code <= 0x7F {
		return 1
	} else if code <= 0x7FF {
		return 2
	} else if 0xD800 <= code && code <= 0xDFFF {
		// between min and max for surrogates
		return -1
	} else if code <= 0xFFFF {
		return 3
	} else if code <= 0x10FFFF {
		// 0x10FFFF is the maximum valid unicode code point
		return 4
	}
	return -1
}
