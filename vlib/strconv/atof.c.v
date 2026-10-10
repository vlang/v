module strconv

// Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file contains utilities for converting a string to a f64 variable.
// IEEE 754 standard is used.
// Know limitation: limited to 18 significant digits
//
// The code is inspired by:
// Grzegorz Kraszewski krashan@teleinfo.pb.edu.pl
// URL: http://krashan.ppa.pl/articles/stringtofloat/
// Original license: MIT
// 96 bit operation utilities
//
// Note: when u128 will be available, these function can be refactored.

// f32 constants
pub const single_plus_zero = u32(0x0000_0000)
pub const single_minus_zero = u32(0x8000_0000)
pub const single_plus_infinity = u32(0x7F80_0000)
pub const single_minus_infinity = u32(0xFF80_0000)

// f64 constants
pub const digits = 18
pub const double_plus_zero = u64(0x0000000000000000)
pub const double_minus_zero = u64(0x8000000000000000)
pub const double_plus_infinity = u64(0x7FF0000000000000)
pub const double_minus_infinity = u64(0xFFF0000000000000)

// char constants
pub const c_dpoint = `.`
pub const c_plus = `+`
pub const c_minus = `-`
pub const c_zero = `0`
pub const c_nine = `9`
pub const c_ten = u32(10)

// right logical shift 96 bit
fn lsr96(s2 u32, s1 u32, s0 u32) (u32, u32, u32) {
	mut r0 := u32(0)
	mut r1 := u32(0)
	mut r2 := u32(0)
	r0 = (s0 >> 1) | ((s1 & u32(1)) << 31)
	r1 = (s1 >> 1) | ((s2 & u32(1)) << 31)
	r2 = s2 >> 1
	return r2, r1, r0
}

// left logical shift 96 bit
fn lsl96(s2 u32, s1 u32, s0 u32) (u32, u32, u32) {
	mut r0 := u32(0)
	mut r1 := u32(0)
	mut r2 := u32(0)
	r2 = (s2 << 1) | ((s1 & (u32(1) << 31)) >> 31)
	r1 = (s1 << 1) | ((s0 & (u32(1) << 31)) >> 31)
	r0 = s0 << 1
	return r2, r1, r0
}

// sum on 96 bit
fn add96(s2 u32, s1 u32, s0 u32, d2 u32, d1 u32, d0 u32) (u32, u32, u32) {
	mut w := u64(0)
	mut r0 := u32(0)
	mut r1 := u32(0)
	mut r2 := u32(0)
	w = u64(s0) + u64(d0)
	r0 = u32(w)
	w >>= 32
	w += u64(s1) + u64(d1)
	r1 = u32(w)
	w >>= 32
	w += u64(s2) + u64(d2)
	r2 = u32(w)
	return r2, r1, r0
}

// subtraction on 96 bit
fn sub96(s2 u32, s1 u32, s0 u32, d2 u32, d1 u32, d0 u32) (u32, u32, u32) {
	mut w := u64(0)
	mut r0 := u32(0)
	mut r1 := u32(0)
	mut r2 := u32(0)
	w = u64(s0) - u64(d0)
	r0 = u32(w)
	w >>= 32
	w += u64(s1) - u64(d1)
	r1 = u32(w)
	w >>= 32
	w += u64(s2) - u64(d2)
	r2 = u32(w)
	return r2, r1, r0
}

// Utility functions
fn is_digit(x u8) bool {
	return x >= c_zero && x <= c_nine
}

fn is_space(x u8) bool {
	return x == `\t` || x == `\n` || x == `\v` || x == `\f` || x == `\r` || x == ` `
}

fn is_exp(x u8) bool {
	return x == `E` || x == `e`
}

// Possible parser return values.
enum ParserState {
	ok             // parser finished OK
	pzero          // no digits or number is smaller than +-2^-1022
	mzero          // number is negative, module smaller
	pinf           // number is higher than +HUGE_VAL
	minf           // number is lower than -HUGE_VAL
	invalid_number // invalid number, used for '#@%^' for example
	extra_char     // extra char after number
}

// is_float_digit accepts decimal digits and separators between two digits.
fn is_float_digit(s string, i int) bool {
	return s[i].is_digit() || (s[i] == `_` && i > 0 && i + 1 < s.len
		&& s[i - 1].is_digit() && s[i + 1].is_digit())
}

// parser tries to parse the given string into a number
@[direct_array_access]
fn parser(s string) (ParserState, PrepNumber) {
	mut digx := 0
	mut result := ParserState.ok
	mut expneg := false
	mut expexp := 0
	mut i := 0
	mut pn := PrepNumber{}

	// A sign may appear only once, and must be followed by a mantissa.
	if s[0] == `-` || s[0] == `+` {
		pn.negative = s[0] == `-`
		i++
	}
	if i == s.len {
		return .invalid_number, pn
	}
	mut has_mantissa_digit := false

	// read mantissa
	for i < s.len && is_float_digit(s, i) {
		if s[i] == `_` {
			i++
			continue
		}
		has_mantissa_digit = true
		if pn.mantissa == 0 && s[i] == c_zero {
			i++
			continue
		}
		// println("${i} => ${s[i]}")
		if digx < digits {
			pn.mantissa *= 10
			pn.mantissa += u64(s[i] - c_zero)
			digx++
		} else if pn.exponent < 2147483647 {
			pn.exponent++
		}
		i++
	}

	// read mantissa decimals
	if i < s.len && s[i] == `.` {
		i++
		for i < s.len && is_float_digit(s, i) {
			if s[i] == `_` {
				i++
				continue
			}
			has_mantissa_digit = true
			if pn.mantissa == 0 && s[i] == c_zero {
				pn.exponent--
				i++
				continue
			}
			if digx < digits {
				pn.mantissa *= 10
				pn.mantissa += u64(s[i] - c_zero)
				pn.exponent--
				digx++
			}
			i++
		}
	}

	if !has_mantissa_digit {
		return .invalid_number, pn
	}

	// read exponent
	if i < s.len && (s[i] == `e` || s[i] == `E`) {
		i++
		if i < s.len {
			// esponent sign
			if s[i] == c_plus {
				i++
			} else if s[i] == c_minus {
				expneg = true
				i++
			}
		}
		mut has_exponent_digit := false
		for i < s.len && is_float_digit(s, i) {
			if s[i] == `_` {
				i++
				continue
			}
			has_exponent_digit = true
			if expexp < 214748364 {
				expexp *= 10
				expexp += int(s[i] - c_zero)
			}
			i++
		}
		if !has_exponent_digit {
			return .invalid_number, pn
		}
	}

	if expneg {
		expexp = -expexp
	}
	pn.exponent += expexp
	if pn.mantissa == 0 {
		if pn.negative {
			result = .mzero
		} else {
			result = .pzero
		}
	} else if pn.exponent > 309 {
		if pn.negative {
			result = .minf
		} else {
			result = .pinf
		}
	} else if pn.exponent < -342 {
		if pn.negative {
			result = .mzero
		} else {
			result = .pzero
		}
	}
	if i == 0 && s.len > 0 {
		return ParserState.invalid_number, pn
	}
	if i != s.len {
		return ParserState.extra_char, pn
	}
	return result, pn
}

// converter returns a u64 with the bit image of the f64 number
fn converter(mut pn PrepNumber) u64 {
	// Trailing characters replace the parser's zero/infinity state with .extra_char.
	// Apply the same range limits here before iterating over the decimal exponent.
	if pn.mantissa == 0 || pn.exponent < -342 {
		return if pn.negative { double_minus_zero } else { double_plus_zero }
	}
	if pn.exponent > 309 {
		return if pn.negative { double_minus_infinity } else { double_plus_infinity }
	}
	mut binexp := 92
	mut sticky := false
	// s0,s1,s2 are the parts of a 96-bit precision integer
	mut s2 := u32(0)
	mut s1 := u32(0)
	mut s0 := u32(0)
	// q0,q1,q2 are the parts of a 96-bit precision integer
	mut q2 := u32(0)
	mut q1 := u32(0)
	mut q0 := u32(0)
	// r0,r1,r2 are the parts of a 96-bit precision integer
	mut r2 := u32(0)
	mut r1 := u32(0)
	mut r0 := u32(0)

	mask28 := u32(u64(0xF) << 28)
	mut result := u64(0)
	// working on 3 u32 to have 96 bit precision
	s0 = u32(pn.mantissa & u64(0x00000000FFFFFFFF))
	s1 = u32(pn.mantissa >> 32)
	s2 = u32(0)
	// so we take the decimal exponent off
	for pn.exponent > 0 {
		q2, q1, q0 = lsl96(s2, s1, s0) // q = s * 2
		r2, r1, r0 = lsl96(q2, q1, q0) // r = s * 4 <=> q * 2
		s2, s1, s0 = lsl96(r2, r1, r0) // s = s * 8 <=> r * 2
		s2, s1, s0 = add96(s2, s1, s0, q2, q1, q0) // s = (s * 8) + (s * 2) <=> s*10
		pn.exponent--
		for (s2 & mask28) != 0 {
			sticky = sticky || s0 & 1 != 0
			q2, q1, q0 = lsr96(s2, s1, s0)
			binexp++
			s2 = q2
			s1 = q1
			s0 = q0
		}
	}
	for pn.exponent < 0 {
		for !((s2 & (u32(1) << 31)) != 0) {
			q2, q1, q0 = lsl96(s2, s1, s0)
			binexp--
			s2 = q2
			s1 = q1
			s0 = q0
		}
		q2 = s2 / c_ten
		r1 = s2 % c_ten
		r2 = (s1 >> 8) | (r1 << 24)
		q1 = r2 / c_ten
		r1 = r2 % c_ten
		r2 = ((s1 & u32(0xFF)) << 16) | (s0 >> 16) | (r1 << 24)
		r0 = r2 / c_ten
		r1 = r2 % c_ten
		q1 = (q1 << 8) | ((r0 & u32(0x00FF0000)) >> 16)
		q0 = r0 << 16
		r2 = (s0 & u32(0xFFFF)) | (r1 << 16)
		q0 |= r2 / c_ten
		sticky = sticky || r2 % c_ten != 0
		s2 = q2
		s1 = q1
		s0 = q0
		pn.exponent++
	}
	// C.printf(c"mantissa before normalization: %08x%08x%08x binexp: %d \n", s2,s1,s0,binexp)
	// normalization, the 28 bit in s2 must the leftest one in the variable
	if s2 != 0 || s1 != 0 || s0 != 0 {
		for (s2 & mask28) == 0 {
			q2, q1, q0 = lsl96(s2, s1, s0)
			binexp--
			s2 = q2
			s1 = q1
			s0 = q0
		}
	}

	// Handle subnormal (denormalized) numbers - very small numbers near zero
	//
	// Normal floats have an implicit leading 1 bit in their mantissa (like 1.xxxxx).
	// When numbers get too small (binexp < -1022), we can't represent them normally.
	// Instead, we use subnormals: set exponent to 0 and shift the mantissa right,
	// losing precision gradually. This prevents abrupt underflow to zero.
	//
	// Example: 1.23e-308 is smaller than the minimum normal float, so we:
	// 1. Keep the normalized mantissa from s2 and s1
	// 2. Shift it right to "denormalize" it (the leading 1 moves into the mantissa)
	// 3. Round correctly using the bits that were shifted out
	// 4. Return with exponent = 0 (subnormal marker)
	if binexp < -1022 && (s2 | s1) != 0 {
		shift := -1022 - binexp
		if shift > 53 {
			return if pn.negative { double_minus_zero } else { double_plus_zero }
		}
		upper := (u64(s2) << 32) | u64(s1)
		round_bit := u64(1) << u32(shift + 7)
		mantissa := upper >> u32(shift + 8)
		round_up := upper & round_bit != 0
			&& (upper & (round_bit - 1) != 0 || s0 != 0 || sticky || mantissa & 1 != 0)
		// A carry into bit 52 is the smallest normal number, rather than zero.
		return (mantissa + u64(round_up)) | (u64(pn.negative) << 63)
	}

	// rounding if needed
	/*
	* "round half to even" algorithm
	* Example for f32, just a reminder
	*
	* If bit 54 is 0, round down
	* If bit 54 is 1
	*	If any bit beyond bit 54 is 1, round up
	*	If all bits beyond bit 54 are 0 (meaning the number is halfway between two floating-point numbers)
	*		If bit 53 is 0, round down
	*		If bit 53 is 1, round up
	*/
	/*
	test case 1 complete
	s2=0x1FFFFFFF
	s1=0xFFFFFF80
	s0=0x0
	*/

	/*
	test case 1 check_round_bit
	s2=0x18888888
	s1=0x88888880
	s0=0x0
	*/

	/*
	test case  check_round_bit + normalization
	s2=0x18888888
	s1=0x88888F80
	s0=0x0
	*/

	// C.printf(c"mantissa before rounding: %08x%08x%08x binexp: %d \n", s2,s1,s0,binexp)
	// s1 => 0xFFFFFFxx only F are represented
	nbit := 7
	check_round_bit := u32(1) << u32(nbit)
	check_round_mask := u32(0xFFFFFFFF) << u32(nbit)
	if (s1 & check_round_bit) != 0 {
		// C.printf(c"need round!! check mask: %08x\n", s1 & ~check_round_mask )
		if (s1 & ~check_round_mask) != 0 || s0 != 0 || sticky {
			// C.printf(c"Add 1!\n")
			s2, s1, s0 = add96(s2, s1, s0, 0, check_round_bit, 0)
		} else {
			// C.printf(c"All 0!\n")
			if (s1 & (check_round_bit << u32(1))) != 0 {
				// C.printf(c"Add 1 form -1 bit control!\n")
				s2, s1, s0 = add96(s2, s1, s0, 0, check_round_bit, 0)
			}
		}
		s1 = s1 & check_round_mask
		s0 = u32(0)
		// recheck normalization
		if s2 & (mask28 << u32(1)) != 0 {
			// C.printf(c"Renormalize!!\n")
			q2, q1, q0 = lsr96(s2, s1, s0)
			binexp++
			// dump(binexp)
			s2 = q2
			s1 = q1
			s0 = q0
		}
	}
	// tmp := ( u64(s2 & ~mask28) << 24) | ((u64(s1) + u64(128)) >> 8)
	// C.printf(c"mantissa after rounding : %08x %08x %08x binexp: %d \n", s2,s1,s0,binexp)
	// C.printf(c"Tmp result: %016x\n",tmp)
	// end rounding
	// offset the binary exponent IEEE 754
	binexp += 1023
	if binexp > 2046 {
		if pn.negative {
			result = double_minus_infinity
		} else {
			result = double_plus_infinity
		}
	} else if binexp < 1 {
		// Should not reach here for subnormals anymore (handled earlier)
		// This is now only for true zeros
		if pn.negative {
			result = double_minus_zero
		} else {
			result = double_plus_zero
		}
	} else if s2 != 0 {
		mut q := u64(0)
		binexs2 := u64(binexp) << 52
		// The guard and sticky bits above have already rounded the significand.
		q = (u64(s2 & ~mask28) << 24) | (u64(s1) >> 8) | binexs2
		if pn.negative {
			q |= (u64(1) << 63)
		}
		result = q
	}
	return result
}

@[params]
pub struct AtoF64Param {
pub:
	allow_extra_chars bool // allow extra characters after number
	allow_overflow    bool // allow numbers too large for an f64, returning +inf or -inf for them
}

// atof64 parses a decimal string into an f64, including case-insensitive NaN
// and signed Inf or Infinity. Underscores may separate digits. Whitespace,
// missing mantissa or exponent digits, and other invalid syntax return errors.
// A number whose magnitude is too large for an f64 returns a `value out of range`
// error. Set allow_overflow to get +inf or -inf for such a number instead.
// A number too small for an f64 is not an error: it rounds to a subnormal value
// or to a signed zero.
// Conversion retains up to 18 significant decimal digits and rounds binary ties to even.
// Set allow_extra_chars to accept trailing characters after a decimal number.
pub fn atof64(s string, param AtoF64Param) !f64 {
	if s.len == 0 {
		return error('expected a number found an empty string')
	}
	mut res := Float64u{}
	special_start := if s[0] == `+` || s[0] == `-` { 1 } else { 0 }
	if special_start < s.len && byte_to_lower(s[special_start]) in [`i`, `n`] {
		// The union reinterprets IEEE 754 special-value bit patterns as f64 values.
		match s.to_lower() {
			'inf', '+inf', 'infinity', '+infinity' {
				res.u = double_plus_infinity
				return unsafe { res.f }
			}
			'-inf', '-infinity' {
				res.u = double_minus_infinity
				return unsafe { res.f }
			}
			'nan' {
				res.u = u64(0x7FF8000000000000)
				return unsafe { res.f }
			}
			else {}
		}
	}
	res_parsing, mut pn := parser(s)
	mut bits := u64(0)
	match res_parsing {
		.ok {
			bits = converter(mut pn)
		}
		.pzero {
			bits = double_plus_zero
		}
		.mzero {
			bits = double_minus_zero
		}
		.pinf {
			bits = double_plus_infinity
		}
		.minf {
			bits = double_minus_infinity
		}
		.extra_char {
			if param.allow_extra_chars {
				bits = converter(mut pn)
			} else {
				return error('extra char after number')
			}
		}
		.invalid_number {
			return error('not a number')
		}
	}
	// The infinity spellings returned above, so infinity bits here, from the parser
	// state or from the converter, mean that the number is too large for an f64.
	if !param.allow_overflow && bits in [double_plus_infinity, double_minus_infinity] {
		return error('strconv.atof64: parsing "${s}": value out of range')
	}
	res.u = bits

	return unsafe { res.f }
}
