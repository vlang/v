// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

import strings

// This was never working correctly, the issue is now
// fixed however the type checks in checker need to be
// updated. if you uncomment it you will see the issue
// type rune = int

// str converts a rune to string.
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

// string converts a rune array to a string.
@[manualfree]
pub fn (ra []rune) string() string {
	mut sb := strings.new_builder(ra.len)
	sb.write_runes(ra)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// repeat returns a new string with `count` number of copies of the rune it was called on.
pub fn (c rune) repeat(count int) string {
	if count <= 0 {
		return ''
	} else if count == 1 {
		return c.str()
	}
	mut buffer := [5]u8{}
	res := unsafe { utf32_to_str_no_malloc(u32(c), mut &buffer[0]) }
	return res.repeat(count)
}

// bytes converts a rune to an array of bytes.
@[manualfree]
pub fn (c rune) bytes() []u8 {
	mut res := []u8{cap: 5}
	mut buf := &u8(res.data)
	res.len = unsafe { utf32_decode_to_buffer(u32(c), mut buf) }
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

// `to_upper` convert to uppercase mode.
pub fn (c rune) to_upper() rune {
	if c < 0x80 {
		if c >= `a` && c <= `z` {
			return c - 32
		}
		return c
	}
	return c.map_to(.to_upper)
}

// `to_lower` convert to lowercase mode.
pub fn (c rune) to_lower() rune {
	if c < 0x80 {
		if c >= `A` && c <= `Z` {
			return c + 32
		}
		return c
	}
	return c.map_to(.to_lower)
}

// `to_title` convert to title mode.
pub fn (c rune) to_title() rune {
	if c < 0x80 {
		if c >= `a` && c <= `z` {
			return c - 32
		}
		return c
	}
	return c.map_to(.to_title)
}

// `map_to` rune map mode: .to_upper/.to_lower/.to_title
@[direct_array_access]
fn (c rune) map_to(mode MapMode) rune {
	mut start := 0
	mut end := rune_maps.len / rune_maps_columns_in_row
	// Binary search
	for start < end {
		middle := (start + end) / 2
		cur_map := unsafe { &rune_maps[middle * rune_maps_columns_in_row] }
		if c >= u32(unsafe { *cur_map }) && c <= u32(unsafe { *(cur_map + 1) }) {
			offset := if mode in [.to_upper, .to_title] {
				unsafe { *(cur_map + 2) }
			} else {
				unsafe { *(cur_map + 3) }
			}
			if offset == rune_maps_ul {
				// upper, lower, upper, lower, ... sequence
				cnt := (c - unsafe { *cur_map }) % 2
				if mode == .to_lower {
					return c + 1 - cnt
				}
				return c - cnt
			} else if offset == rune_maps_utl {
				// upper, title, lower, upper, title, lower, ... sequence
				cnt := (c - unsafe { *cur_map }) % 3
				if mode == .to_upper {
					return c - cnt
				} else if mode == .to_lower {
					return c + 2 - cnt
				}
				return c + 1 - cnt
			}
			return c + offset
		}
		if c < u32(unsafe { *cur_map }) {
			end = middle
		} else {
			start = middle + 1
		}
	}
	return c
}
