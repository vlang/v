module strconv

// Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
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
pub const (
	single_plus_zero      = u32(0x0000_0000)
	single_minus_zero     = u32(0x8000_0000)
	single_plus_infinity  = u32(0x7F80_0000)
	single_minus_infinity = u32(0xFF80_0000)
)

// f64 constants
pub const (
	digits                = 18
	double_plus_zero      = u64(0x0000000000000000)
	double_minus_zero     = u64(0x8000000000000000)
	double_plus_infinity  = u64(0x7FF0000000000000)
	double_minus_infinity = u64(0xFFF0000000000000)
)

// char constants
pub const (
	c_dpoint = `.`
	c_plus   = `+`
	c_minus  = `-`
	c_zero   = `0`
	c_nine   = `9`
	c_ten    = u32(10)
)

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
	return (x >= strconv.c_zero && x <= strconv.c_nine) == true
}

fn is_space(x u8) bool {
	return x == `\t` || x == `\n` || x == `\v` || x == `\f` || x == `\r` || x == ` `
}

fn is_exp(x u8) bool {
	return (x == `E` || x == `e`) == true
}

// Possible parser return values.
enum ParserState {
	ok // parser finished OK
	pzero // no digits or number is smaller than +-2^-1022
	mzero // number is negative, module smaller
	pinf // number is higher than +HUGE_VAL
	minf // number is lower than -HUGE_VAL
	invalid_number // invalid number, used for '#@%^' for example
}

// parser tries to parse the given string into a number
// NOTE: #TOFIX need one char after the last char of the number
fn parser(s string) (ParserState, PrepNumber) {
	mut digx := 0
	mut result := ParserState.ok
	mut expneg := false
	mut expexp := 0
	mut i := 0
	mut pn := PrepNumber{}

	// skip spaces
	for i < s.len && s[i].is_space() {
		i++
	}

	// check negatives
	if s[i] == `-` {
		pn.negative = true
		i++
	}

	// positive sign ignore it
	if s[i] == `+` {
		i++
	}

	// read mantissa
	for i < s.len && s[i].is_digit() {
		// println("$i => ${s[i]}")
		if digx < strconv.digits {
			pn.mantissa *= 10
			pn.mantissa += u64(s[i] - strconv.c_zero)
			digx++
		} else if pn.exponent < 2147483647 {
			pn.exponent++
		}
		i++
	}

	// read mantissa decimals
	if (i < s.len) && (s[i] == `.`) {
		i++
		for i < s.len && s[i].is_digit() {
			if digx < strconv.digits {
				pn.mantissa *= 10
				pn.mantissa += u64(s[i] - strconv.c_zero)
				pn.exponent--
				digx++
			}
			i++
		}
	}

	// read exponent
	if (i < s.len) && ((s[i] == `e`) || (s[i] == `E`)) {
		i++
		if i < s.len {
			// esponent sign
			if s[i] == strconv.c_plus {
				i++
			} else if s[i] == strconv.c_minus {
				expneg = true
				i++
			}

			for i < s.len && s[i].is_digit() {
				if expexp < 214748364 {
					expexp *= 10
					expexp += int(s[i] - strconv.c_zero)
				}
				i++
			}
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
	} else if pn.exponent < -328 {
		if pn.negative {
			result = .mzero
		} else {
			result = .pzero
		}
	}
	if i == 0 && s.len > 0 {
		return ParserState.invalid_number, pn
	}
	return result, pn
}

// converter returns a u64 with the bit image of the f64 number
fn converter(mut pn PrepNumber) u64 {
	mut binexp := 92
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
	//
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
		q2 = s2 / strconv.c_ten
		r1 = s2 % strconv.c_ten
		r2 = (s1 >> 8) | (r1 << 24)
		q1 = r2 / strconv.c_ten
		r1 = r2 % strconv.c_ten
		r2 = ((s1 & u32(0xFF)) << 16) | (s0 >> 16) | (r1 << 24)
		r0 = r2 / strconv.c_ten
		r1 = r2 % strconv.c_ten
		q1 = (q1 << 8) | ((r0 & u32(0x00FF0000)) >> 16)
		q0 = r0 << 16
		r2 = (s0 & u32(0xFFFF)) | (r1 << 16)
		q0 |= r2 / strconv.c_ten
		s2 = q2
		s1 = q1
		s0 = q0
		pn.exponent++
	}
	// C.printf("mantissa before normalization: %08x%08x%08x binexp: %d \n", s2,s1,s0,binexp)
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

	// C.printf("mantissa before rounding: %08x%08x%08x binexp: %d \n", s2,s1,s0,binexp)
	// s1 => 0xFFFFFFxx only F are rapresented
	nbit := 7
	check_round_bit := u32(1) << u32(nbit)
	check_round_mask := u32(0xFFFFFFFF) << u32(nbit)
	if (s1 & check_round_bit) != 0 {
		// C.printf("need round!! cehck mask: %08x\n", s1 & ~check_round_mask )
		if (s1 & ~check_round_mask) != 0 {
			// C.printf("Add 1!\n")
			s2, s1, s0 = add96(s2, s1, s0, 0, check_round_bit, 0)
		} else {
			// C.printf("All 0!\n")
			if (s1 & (check_round_bit << u32(1))) != 0 {
				// C.printf("Add 1 form -1 bit control!\n")
				s2, s1, s0 = add96(s2, s1, s0, 0, check_round_bit, 0)
			}
		}
		s1 = s1 & check_round_mask
		s0 = u32(0)
		// recheck normalization
		if s2 & (mask28 << u32(1)) != 0 {
			// C.printf("Renormalize!!")
			q2, q1, q0 = lsr96(s2, s1, s0)
			binexp--
			s2 = q2
			s1 = q1
			s0 = q0
		}
	}
	// tmp := ( u64(s2 & ~mask28) << 24) | ((u64(s1) + u64(128)) >> 8)
	// C.printf("mantissa after rounding : %08x%08x%08x binexp: %d \n", s2,s1,s0,binexp)
	// C.printf("Tmp result: %016x\n",tmp)
	// end rounding
	// offset the binary exponent IEEE 754
	binexp += 1023
	if binexp > 2046 {
		if pn.negative {
			result = strconv.double_minus_infinity
		} else {
			result = strconv.double_plus_infinity
		}
	} else if binexp < 1 {
		if pn.negative {
			result = strconv.double_minus_zero
		} else {
			result = strconv.double_plus_zero
		}
	} else if s2 != 0 {
		mut q := u64(0)
		binexs2 := u64(binexp) << 52
		q = (u64(s2 & ~mask28) << 24) | ((u64(s1) + u64(128)) >> 8) | binexs2
		if pn.negative {
			q |= (u64(1) << 63)
		}
		result = q
	}
	return result
}

// atof64 parses the string `s`, and if possible, converts it into a f64 number
pub fn atof64(s string) ?f64 {
	if s.len == 0 {
		return error('expected a number found an empty string')
	}
	mut res := Float64u{}
	mut res_parsing, mut pn := parser(s)
	match res_parsing {
		.ok {
			res.u = converter(mut pn)
		}
		.pzero {
			res.u = strconv.double_plus_zero
		}
		.mzero {
			res.u = strconv.double_minus_zero
		}
		.pinf {
			res.u = strconv.double_plus_infinity
		}
		.minf {
			res.u = strconv.double_minus_infinity
		}
		.invalid_number {
			return error('not a number')
		}
	}
	return unsafe { res.f }
}
