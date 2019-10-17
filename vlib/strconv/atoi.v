// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// TODO: use optionals, or some way to return default with error.

module strconv

const(
    // int_size is the size in bits of an int or uint value.
    // int_size = 32 << (~u32(0) >> 63)
	int_size = 32
    max_u64 = u64(u64(1<<64) - 1)
)

fn byte_to_lower(c byte) byte {
    return c | (`x` - `X`)
}

// parse_uint is like parse_int but for unsigned numbers.
pub fn parse_uint(_s string, _base int, _bit_size int) u64 {
	mut s := _s.trim_space()
    mut bit_size := _bit_size
    mut base := _base

	if s == "" || !underscore_ok(s) {
		// return error('parse_uint: syntax error $s')
        return u64(0)
	}
    // base  := 0
	base0 := base == 0
	s0 := s
	if 2 <= base && base <= 36 {
		// valid base; nothing to do
	} else if base == 0 {
		// Look for octal, hex prefix.
        base = 10
		if s[0] == `0` {
			if s.len >= 3 && byte_to_lower(s[1]) == `b` { 
                base = 2 
                s = s.right(2) 
            }
			else if s.len >= 3 && byte_to_lower(s[1]) == `o` {
                base = 8
                s = s.right(2)
			}
			else if s.len >= 3 && byte_to_lower(s[1]) == `x` {
                base = 16
                s = s.right(2)
			}
			else {
                base = 8
                s = s.right(1)
            }
		}
	} else {
		// return error('parse_uint: base error $s0 - $base')
        return u64(0)
	}

	if bit_size == 0 {
		bit_size = int(int_size)
	} else if bit_size < 0 || bit_size > 64 {
		// return error('parse_uint: bitsize error $s0 - $bit_size')
		return u64(0)
	}
    
	// Cutoff is the smallest number such that cutoff*base > maxUint64.
	// Use compile-time constants for common cases.
	mut cutoff := u64(0)
    cutoff = u64(max_u64/u64(base)) + u64(1)
	mut max_val := u64(0)
    if bit_size == 64 {
        bs64 := u64(bit_size)
		a := u64(1)<<bs64
        max_val = u64(a) - u64(1)
    } else {
		bs32 := u32(bit_size)
        max_val = u32(1)<<u32(bs32 - u32(1))
    }

	mut underscores := false
	mut n := u64(0)
	for _, c in s {
		mut d := byte(0)
        cl := byte_to_lower(c)
		if c == `_` && base0 {
			// underscore_ok already called
			underscores = true
			continue
        }
        else if `0` <= c && c <= `9`   { d = c - `0` }
        else if `a` <= cl && cl <= `z` { d = cl - `a` + 10 }
        else {
			// return error('parse_uint: syntax error $s0')
            return u64(0)
		}
		if d >= byte(base) {
			// return error('parse_uint: syntax error $s0')
			return u64(0)
		}
		if n >= cutoff {
			// n*base overflows
			// return error('parse_uint: range error $s0')
            return max_val
		}
		n *= u64(base)
		n1 := n + u64(d)
		if n1 < n || n1 > u64(max_val) {
            // n+v overflows
			// return error('parse_uint: range error $s0')
            return max_val
		}
		n = n1
	}
	if underscores && !underscore_ok(s0) {
			// return error('parse_uint: syntax error $s0')
			return u64(0)
	}

    return n
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
	mut s := _s
    mut bit_size := _bit_size
    
    // const fnparse_int = "parse_int"
	if s == '' {
		// return error('parse_int: syntax error $s')
        return i64(0)
	}
	// Pick off leading sign.
	s0 := s
	mut neg := false
	if s[0] == `+` {
		s = s.right(1)
	} else if s[0] == `-` {
		neg = true
		s = s.right(1)
	}

	// Convert unsigned and check range.
	// un := parse_uint(s, base, bit_size) or {
    //     return i64(0)
    // }
	un := parse_uint(s, base, bit_size)
	if un == 0 {
		return i64(0)
	}


	if bit_size == 0 {
		bit_size = int(int_size)
	}

	// TODO: check should u64(bit_size-1) be size of int (32)?
	cutoff := u64(u64(1) << u64(bit_size-1))
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


// atoi is equivalent to parse_int(s, 10, 0), converted to type int.
pub fn atoi(_s string) int {
	mut s := _s
    // const fnAtoi = "Atoi"

	if (int_size == 32 && (0 < s.len && s.len < 10)) ||
		(int_size == 64 && (0 < s.len && s.len < 19)) {
		// Fast path for small integers that fit int type.
		s0 := s
		if s[0] == `-` || s[0] == `+` {
			s = s.right(1)
			if s.len < 1 {
				// return 0, &NumError{fnAtoi, s0, ErrSyntax}
                return 0
			}
		}

		mut n := 0
		for _, ch0 in s {
            ch :=  ch0 - `0`
			if ch > 9 {
				// return 0, &NumError{fnAtoi, s0, ErrSyntax}
                return 0
			}
			n = n*10 + int(ch)
		}

		return if s0[0] == `-` { -n } else { n }
	}

	// Slow path for invalid, big, or underscored integers.
	int64 := parse_int(s, 10, 0)

	return int(int64)
}

// underscore_ok reports whether the underscores in s are allowed.
// Checking them in this one function lets all the parsers skip over them simply.
// Underscore must appear only between digits or between a base prefix and a digit.
fn underscore_ok(_s string) bool {
	mut s := _s
	// saw tracks the last character (class) we saw:
	// ^ for beginning of number,
	// 0 for a digit or base prefix,
	// _ for an underscore,
	// ! for none of the above.
	mut saw := `^`
	mut i := 0

	// Optional sign.
	if s.len >= 1 && (s[0] == `-` || s[0] == `+`) {
		s = s.right(1)
	}

	// Optional base prefix.
	mut hex := false
	if s.len >= 2 && s[0] == `0` && (byte_to_lower(s[1]) == `b` || byte_to_lower(s[1]) == `o` || byte_to_lower(s[1]) == `x`) {
		i = 2
		saw = `0` // base prefix counts as a digit for "underscore as digit separator"
		hex = byte_to_lower(s[1]) == `x`
	}

	// Number proper.
	for ; i < s.len; i++ {
		// Digits are always okay.
		if (`0` <= s[i] && s[i] <= `9`) || (hex && `a` <= byte_to_lower(s[i]) && byte_to_lower(s[i]) <= `f`) {
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
