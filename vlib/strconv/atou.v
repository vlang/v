// Copyright (c) 2019-2024 V language community. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module strconv

// atou_common_check perform basics check on unsigned string to parse.
// Test emptiness, + sign presence, absence of minus sign, presence of digit after
// signs and no underscore as first character.
// returns s first digit index or an error.
@[direct_array_access]
fn atou_common_check(s string) !int {
	if s == '' {
		return error('strconv.atou: parsing "": empty string')
	}

	mut start_idx := 0

	if s[0] == `-` {
		return error('strconv.atou: parsing "{s}" : negative value')
	}

	if s[0] == `+` {
		start_idx++
	}

	if s.len - start_idx < 1 {
		return error('strconv.atou: parsing "${s}": no number after sign')
	}

	if s[start_idx] == `_` || s[s.len - 1] == `_` {
		return error('strconv.atou: parsing "${s}": values cannot start or end with underscores')
	}
	return start_idx
}

// atou_common performs computation for all u8, u16 and u32 type, excluding i64.
// Parse values, and returns consistent error message over differents types.
// s is string to parse, max is respective types max value.
@[direct_array_access]
fn atou_common(s string, type_max u64) !u64 {
	mut start_idx := atou_common_check(s)!
	mut x := u64(0)
	mut underscored := false
	for i in start_idx .. s.len {
		c := s[i] - `0`
		if c == 47 { // 47 = Ascii(`_`) -  ascii(`0`) = 95 - 48.
			if underscored == true { // Two consecutives underscore
				return error('strconv.atou: parsing "${s}": consecutives underscores are not allowed')
			}
			underscored = true
			continue // Skip underscore
		} else {
			if c > 9 {
				return error('strconv.atou: parsing "${s}": invalid radix 10 character')
			}
			underscored = false

			oldx := x
			x = (x * 10) + u64(c)
			if x > type_max || oldx > x {
				return error('strconv.atou: parsing "${s}": integer overflow')
			}
		}
	}
	return x
}

// atou8 is equivalent to parse_uint(s, 10, 0), converted to type u8.
// It returns u8 in range [0..255] or an Error.
pub fn atou8(s string) !u8 {
	return u8(atou_common(s, max_u8)!)
}

// atou16 is equivalent to parse_uint(s, 10, 0), converted to type u16.
// It returns u16 in range [0..65535] or an Error.
pub fn atou16(s string) !u16 {
	return u16(atou_common(s, max_u16)!)
}

// atou is equivalent to parse_uint(s, 10, 0), converted to type u32.
pub fn atou(s string) !u32 {
	return u32(atou_common(s, max_u32)!)
}

// atou32 is identical to atou. Here to provide a symmetrical API with atoi/atoi32
// It returns u32 in range [0..4294967295] or an Error.
pub fn atou32(s string) !u32 {
	return u32(atou_common(s, max_u32)!)
}

// atou64 is equivalent to parse_uint(s, 10, 0), converted to type u64.
// It returns u64 in range [0..18446744073709551615] or an Error.
pub fn atou64(s string) !u64 {
	return u64(atou_common(s, max_u64)!)
}
