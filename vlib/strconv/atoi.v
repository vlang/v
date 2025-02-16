module strconv

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// TODO: use options, or some way to return default with error.
// int_size is the size in bits of an int or uint value.
// int_size = 32 << (~u32(0) >> 63)
// max_u64 = u64(u64(1 << 63) - 1)
const int_size = 32
const max_u64 = u64(18446744073709551615)

@[inline]
pub fn byte_to_lower(c u8) u8 {
	return c | 32
}

// common_parse_uint is called by parse_uint and allows the parsing
// to stop on non or invalid digit characters and return with an error
pub fn common_parse_uint(s string, _base int, _bit_size int, error_on_non_digit bool, error_on_high_digit bool) !u64 {
	result, err := common_parse_uint2(s, _base, _bit_size)
	// TODO: error_on_non_digit and error_on_high_digit have no difference
	if err != 0 && (error_on_non_digit || error_on_high_digit) {
		match err {
			-1 { return error('common_parse_uint: wrong base ${_base} for ${s}') }
			-2 { return error('common_parse_uint: wrong bit size ${_bit_size} for ${s}') }
			-3 { return error('common_parse_uint: integer overflow ${s}') }
			else { return error('common_parse_uint: syntax error ${s}') }
		}
	}
	return result
}

// the first returned value contains the parsed value,
// the second returned value contains the error code (0 = OK, >1 = index of first non-parseable character + 1, -1 = wrong base, -2 = wrong bit size, -3 = overflow)
@[direct_array_access]
pub fn common_parse_uint2(s string, _base int, _bit_size int) (u64, int) {
	if s == '' {
		return u64(0), 1
	}

	mut bit_size := _bit_size
	mut base := _base
	mut start_index := 0

	if base == 0 {
		// Look for octal, binary and hex prefix.
		base = 10
		if s[0] == `0` {
			ch := s[1] | 32
			if s.len >= 3 {
				if ch == `b` {
					base = 2
					start_index += 2
				} else if ch == `o` {
					base = 8
					start_index += 2
				} else if ch == `x` {
					base = 16
					start_index += 2
				}

				// check for underscore after the base prefix
				if s[start_index] == `_` {
					start_index++
				}
			}
			// manage leading zeros in decimal base's numbers
			// otherwise it is an octal for C compatibility
			// TODO: Check if this behaviour is logically right
			else if s.len >= 2 && (s[1] >= `0` && s[1] <= `9`) {
				base = 10
				start_index++
			} else {
				base = 8
				start_index++
			}
		}
	}

	if bit_size == 0 {
		bit_size = int_size
	} else if bit_size < 0 || bit_size > 64 {
		return u64(0), -2
	}
	// Cutoff is the smallest number such that cutoff*base > maxUint64.
	// Use compile-time constants for common cases.
	cutoff := max_u64 / u64(base) + u64(1)
	max_val := if bit_size == 64 { max_u64 } else { (u64(1) << u64(bit_size)) - u64(1) }
	basem1 := base - 1

	mut n := u64(0)
	for i in start_index .. s.len {
		mut c := s[i]

		// manage underscore inside the number
		if c == `_` {
			if i == start_index || i >= (s.len - 1) {
				// println("_ limit")
				return u64(0), 1
			}
			if s[i - 1] == `_` || s[i + 1] == `_` {
				// println("_ *2")
				return u64(0), 1
			}

			continue
		}

		mut sub_count := 0

		// get the 0-9 digit
		c -= 48 // subtract the rune `0`

		// check if we are in the superior base rune interval [A..Z]
		if c >= 17 { // (65 - 48)
			sub_count++
			c -= 7 // subtract the `A` - `0` rune to obtain the value of the digit

			// check if we are in the superior base rune interval [a..z]
			if c >= 42 { // (97 - 7 - 48)
				sub_count++
				c -= 32 // subtract the `a` - `0` rune to obtain the value of the digit
			}
		}

		// check for digit over base
		if c > basem1 || (sub_count == 0 && c > 9) {
			return n, i + 1
		}

		// check if we are in the cutoff zone
		if n >= cutoff {
			// n*base overflows
			// return error('parse_uint: range error $s')
			return max_val, -3
		}
		n *= u64(base)
		n1 := n + u64(c)
		if n1 < n || n1 > max_val {
			// n+v overflows
			// return error('parse_uint: range error $s')
			return max_val, -3
		}
		n = n1
	}
	return n, 0
}

// parse_uint is like parse_int but for unsigned numbers.
pub fn parse_uint(s string, _base int, _bit_size int) !u64 {
	return common_parse_uint(s, _base, _bit_size, true, true)
}

// common_parse_int is called by parse int and allows the parsing
// to stop on non or invalid digit characters and return with an error
@[direct_array_access]
pub fn common_parse_int(_s string, base int, _bit_size int, error_on_non_digit bool, error_on_high_digit bool) !i64 {
	if _s == '' {
		// return error('parse_int: syntax error $s')
		return i64(0)
	}
	mut bit_size := _bit_size
	if bit_size == 0 {
		bit_size = int_size
	}
	mut s := _s
	// Pick off leading sign.
	mut neg := false
	if s[0] == `+` {
		// s = s[1..]
		unsafe {
			s = tos(s.str + 1, s.len - 1)
		}
	} else if s[0] == `-` {
		neg = true
		// s = s[1..]
		unsafe {
			s = tos(s.str + 1, s.len - 1)
		}
	}

	// Convert unsigned and check range.
	// un := parse_uint(s, base, bit_size) or {
	// return i64(0)
	// }
	un := common_parse_uint(s, base, bit_size, error_on_non_digit, error_on_high_digit)!
	if un == 0 {
		return i64(0)
	}
	// TODO: check should u64(bit_size-1) be size of int (32)?
	cutoff := u64(1) << u64(bit_size - 1)
	if !neg && un >= cutoff {
		// return error('parse_int: range error $s0')
		return i64(cutoff - u64(1))
	}
	if neg && un > cutoff {
		// return error('parse_int: range error $s0')
		return -i64(cutoff)
	}
	return if neg { -i64(un) } else { i64(un) }
}

// parse_int interprets a string s in the given base (0, 2 to 36) and
// bit size (0 to 64) and returns the corresponding value i.
//
// If the base argument is 0, the true base is implied by the string's
// prefix: 2 for "0b", 8 for "0" or "0o", 16 for "0x", and 10 otherwise.
// Also, for argument base 0 only, underscore characters are permitted
// as defined by the Go syntax for integer literals.
//
// The bitSize argument specifies the integer type
// that the result must fit into. Bit sizes 0, 8, 16, 32, and 64
// correspond to int, int8, int16, int32, and int64.
// If bitSize is below 0 or above 64, an error is returned.
pub fn parse_int(_s string, base int, _bit_size int) !i64 {
	return common_parse_int(_s, base, _bit_size, true, true)
}

// atoi is equivalent to parse_int(s, 10, 0), converted to type int.
// It follows V scanner as much as observed.
@[direct_array_access]
pub fn atoi32(s string) !int {
	if s == '' {
		return error('strconv.atoi: parsing "": empty string')
	}

	mut start_idx := 0
	mut sign := 1

	if s[0] == `-` || s[0] == `+` {
		start_idx++
		if s[0] == `-` {
			sign = -1
		}
	}

	if s.len - start_idx < 1 {
		return error('strconv.atoi: parsing "${s}": invalid syntax')
	}

	if s[start_idx] == `_` || s[s.len - 1] == `_` {
		return error('strconv.atoi: parsing "${s}": invalid syntax')
	}

	mut x := int(0)
	mut underscored := false
	for i in start_idx .. s.len {
		c := s[i] - `0`
		if c == 47 { // 47 = Ascii(`_`) -  ascii(`0`) = 95 - 48.
			if underscored == true { // Two consecutives underscore
				return error('strconv.atoi: parsing "${s}": consecutives underscores are not allowed')
			}
			underscored = true
			continue // Skip underscore
		} else {
			if c > 9 {
				return error('strconv.atoi: parsing "${s}": invalid radix 10 character')
			}
			underscored = false
			x = safe_mul10_32bits(x) or { return error('strconv.atoi: parsing "${s}": ${err}') }
			x = safe_add_32bits(x, int(c * sign)) or {
				return error('strconv.atoi: parsing "${s}": ${err}')
			}
		}
	}
	println('Final = ${int(x)}')
	return int(x)
}

@[direct_array_access]
pub fn atoi(s string) !int {
	if s == '' {
		return error('strconv.atoi: parsing "": empty string')
	}

	mut start_idx := 0
	mut sign := i64(1)

	if s[0] == `-` || s[0] == `+` {
		start_idx++
		if s[0] == `-` {
			sign = -1
		}
	}

	if s.len - start_idx < 1 {
		return error('strconv.atoi: parsing "${s}": invalid syntax')
	}

	if s[start_idx] == `_` || s[s.len - 1] == `_` {
		return error('strconv.atoi: parsing "${s}": invalid syntax')
	}

	mut x := i64(0)
	mut underscored := false
	for i in start_idx .. s.len {
		c := s[i] - `0`
		if c == 47 { // 47 = Ascii(`_`) -  ascii(`0`) = 95 - 48.
			if underscored == true { // Two consecutives underscore
				return error('strconv.atoi: parsing "${s}": consecutives underscores are not allowed')
			}
			underscored = true
			continue // Skip underscore
		} else {
			if c > 9 {
				return error('strconv.atoi: parsing "${s}":  invalid radix 10 character')
			}
			underscored = false
			x = (x * 10) + (c * sign)
			if sign == 1 && x > i64(max_int) {
				return error('strconv.atoi: parsing "${s}": integer overflow')
			} else {
				if x < i64(min_int) {
					return error('strconv.atoi: parsing "${s}": integer underflow')
				}
			}
		}
	}
	return int(x)
}

// safe_add32 performs a signed 32 bits addition and returns an error
// in case of overflow or underflow.
@[inline]
fn safe_add_32bits(a int, b int) !int {
	if a > 0 && b > (max_int - a) {
		return error('integer overflow')
	} else if a < 0 && b < (min_int - a) {
		return error('integer underflow')
	}
	return int(a + b)
}

// safe_mul10 performs a * 10 multiplication and returns an error
// in case of overflow or underflow.
@[inline]
fn safe_mul10_32bits(a int) !int {
	if a > 0 && a > (max_int / 10) {
		return error('integer overflow')
	}
	if a < 0 && a < (min_int / 10) {
		return error('integer underflow')
	}
	return int(a * 10)
}
