module strconv

// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// TODO: use optionals, or some way to return default with error.
const (
	// int_size is the size in bits of an int or uint value.
	// int_size = 32 << (~u32(0) >> 63)
	// max_u64 = u64(u64(1 << 63) - 1)
	int_size = 32
	max_u64  = u64(18446744073709551615) // as u64 // use this until we add support
)

pub fn byte_to_lower(c byte) byte {
	return c | (`x` - `X`)
}

// common_parse_uint is called by parse_uint and allows the parsing
// to stop on non or invalid digit characters and return the result so far
pub fn common_parse_uint(s string, _base int, _bit_size int, error_on_non_digit bool, error_on_high_digit bool) u64 {
	result, error := common_parse_uint2(s, _base, _bit_size)
	if error != 0 {
		if error > 0 && (error_on_non_digit || error_on_high_digit) {
			return u64(0)
		}
	}
	return result
}

// the first returned value contains the parsed value,
// the second returned value contains the error code (0 = OK, >1 = index of first non-parseable character + 1, -1 = wrong base, -2 = wrong bit size, -3 = overflow)
pub fn common_parse_uint2(s string, _base int, _bit_size int) (u64, int) {
	mut bit_size := _bit_size
	mut base := _base
	if s.len < 1 || !underscore_ok(s) {
		// return error('parse_uint: syntax error $s')
		return u64(0), 1
	}
	base0 := base == 0
	mut start_index := 0
	if 2 <= base && base <= 36 {
		// valid base; nothing to do
	} else if base == 0 {
		// Look for octal, hex prefix.
		base = 10
		if s[0] == `0` {
			if s.len >= 3 && byte_to_lower(s[1]) == `b` {
				base = 2
				start_index += 2
			} else if s.len >= 3 && byte_to_lower(s[1]) == `o` {
				base = 8
				start_index += 2
			} else if s.len >= 3 && byte_to_lower(s[1]) == `x` {
				base = 16
				start_index += 2
			}
			// manage leading zeros in decimal base's numbers
			else if s.len >= 2 && (s[1] >= `0` && s[1] <= `9`) {
				base = 10
				start_index++
			} else {
				base = 8
				start_index++
			}
		}
	} else {
		// return error('parse_uint: base error $s - $base')
		return u64(0), -1
	}
	if bit_size == 0 {
		bit_size = int_size
	} else if bit_size < 0 || bit_size > 64 {
		// return error('parse_uint: bitsize error $s - $bit_size')
		return u64(0), -2
	}
	// Cutoff is the smallest number such that cutoff*base > maxUint64.
	// Use compile-time constants for common cases.
	cutoff := max_u64 / u64(base) + u64(1)
	max_val := if bit_size == 64 { max_u64 } else { (u64(1) << u64(bit_size)) - u64(1) }
	mut n := u64(0)
	for i in start_index .. s.len {
		c := s[i]
		cl := byte_to_lower(c)
		mut d := byte(0)
		if c == `_` && base0 {
			// underscore_ok already called
			continue
		} else if `0` <= c && c <= `9` {
			d = c - `0`
		} else if `a` <= cl && cl <= `z` {
			d = cl - `a` + 10
		} else {
			return n, i + 1
		}
		if d >= byte(base) {
			return n, i + 1
		}
		if n >= cutoff {
			// n*base overflows
			// return error('parse_uint: range error $s')
			return max_val, -3
		}
		n *= u64(base)
		n1 := n + u64(d)
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
pub fn parse_uint(s string, _base int, _bit_size int) u64 {
	return common_parse_uint(s, _base, _bit_size, true, true)
}

// common_parse_int is called by parse int and allows the parsing
// to stop on non or invalid digit characters and return the result so far
pub fn common_parse_int(_s string, base int, _bit_size int, error_on_non_digit bool, error_on_high_digit bool) i64 {
	mut s := _s
	mut bit_size := _bit_size
	if s.len < 1 {
		// return error('parse_int: syntax error $s')
		return i64(0)
	}
	// Pick off leading sign.
	mut neg := false
	if s[0] == `+` {
		s = s[1..]
	} else if s[0] == `-` {
		neg = true
		s = s[1..]
	}
	// Convert unsigned and check range.
	// un := parse_uint(s, base, bit_size) or {
	// return i64(0)
	// }
	un := common_parse_uint(s, base, bit_size, error_on_non_digit, error_on_high_digit)
	if un == 0 {
		return i64(0)
	}
	if bit_size == 0 {
		bit_size = int_size
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
	return if neg {
		-i64(un)
	} else {
		i64(un)
	}
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
pub fn parse_int(_s string, base int, _bit_size int) i64 {
	return common_parse_int(_s, base, _bit_size, true, true)
}

// atoi is equivalent to parse_int(s, 10, 0), converted to type int.
pub fn atoi(s string) ?int {
	if s == '' {
		return error('strconv.atoi: parsing "$s": invalid syntax ')
	}
	if (int_size == 32 && (0 < s.len &&
		s.len < 10)) ||
		(int_size == 64 && (0 < s.len && s.len < 19)) {
		// Fast path for small integers that fit int type.
		mut start_idx := 0
		if s[0] == `-` || s[0] == `+` {
			start_idx++
			if s.len - start_idx < 1 {
				// return 0, &NumError{fnAtoi, s0, ErrSyntax}
				return error('strconv.atoi: parsing "$s": invalid syntax ')
			}
		}
		mut n := 0
		for i in start_idx .. s.len {
			ch := s[i] - `0`
			if ch > 9 {
				// return 0, &NumError{fnAtoi, s0, ErrSyntax}
				return error('strconv.atoi: parsing "$s": invalid syntax ')
			}
			n = n * 10 + int(ch)
		}
		return if s[0] == `-` {
			-n
		} else {
			n
		}
	}
	// Slow path for invalid, big, or underscored integers.
	int64 := parse_int(s, 10, 0)
	return int(int64)
}

// underscore_ok reports whether the underscores in s are allowed.
// Checking them in this one function lets all the parsers skip over them simply.
// Underscore must appear only between digits or between a base prefix and a digit.
fn underscore_ok(s string) bool {
	// saw tracks the last character (class) we saw:
	// ^ for beginning of number,
	// 0 for a digit or base prefix,
	// _ for an underscore,
	// ! for none of the above.
	mut saw := `^`
	mut i := 0
	// Optional sign.
	if s.len >= 1 && (s[0] == `-` || s[0] == `+`) {
		i++
	}
	// Optional base prefix.
	mut hex := false
	if s.len - i >= 2 && s[i] == `0` &&
		(byte_to_lower(s[i + 1]) == `b` || byte_to_lower(s[i + 1]) == `o` || byte_to_lower(s[i + 1]) == `x`) {
		saw = `0` // base prefix counts as a digit for "underscore as digit separator"
		hex = byte_to_lower(s[i + 1]) == `x`
		i += 2
	}
	// Number proper.
	for ; i < s.len; i++ {
		// Digits are always okay.
		if (`0` <= s[i] && s[i] <= `9`) ||
			(hex && `a` <= byte_to_lower(s[i]) && byte_to_lower(s[i]) <= `f`) {
			saw = `0`
			continue
		}
		// Underscore must follow digit.
		if s[i] == `_` {
			if saw != `0` {
				return false
			}
			saw = `_`
			continue
		}
		// Underscore must also be followed by digit.
		if saw == `_` {
			return false
		}
		// Saw non-digit, non-underscore.
		saw = `!`
	}
	return saw != `_`
}
