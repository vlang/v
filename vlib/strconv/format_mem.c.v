/*=============================================================================
Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
=============================================================================*/
module strconv

import strings

// format_str_sb is a `strings.Builder` version of `format_str`.
pub fn format_str_sb(s string, p BF_param, mut sb strings.Builder) {
	if p.len0 <= 0 {
		sb.write_string(s)
		return
	}
	dif := p.len0 - utf8_str_visible_length(s)
	if dif <= 0 {
		sb.write_string(s)
		return
	}

	if p.align == .right {
		for i1 := 0; i1 < dif; i1++ {
			sb.write_u8(p.pad_ch)
		}
	}
	sb.write_string(s)
	if p.align == .left {
		for i1 := 0; i1 < dif; i1++ {
			sb.write_u8(p.pad_ch)
		}
	}
}

const max_size_f64_char = 512 // the f64 max representation is -36,028,797,018,963,968e1023, 21 chars, but alignment padding requires more

// digit pairs in reverse order
const digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'

// format_dec_sb formats an u64 using a `strings.Builder`.
@[direct_array_access]
pub fn format_dec_sb(d u64, p BF_param, mut res strings.Builder) {
	mut n_char := dec_digits(d)
	sign_len := if !p.positive || p.sign_flag { 1 } else { 0 }
	number_len := sign_len + n_char
	dif := p.len0 - number_len
	mut sign_written := false

	if p.align == .right {
		if p.pad_ch == `0` {
			if p.positive {
				if p.sign_flag {
					res.write_u8(`+`)
					sign_written = true
				}
			} else {
				res.write_u8(`-`)
				sign_written = true
			}
		}
		// write the pad chars
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}

	if !sign_written {
		// no pad char, write the sign before the number
		if p.positive {
			if p.sign_flag {
				res.write_u8(`+`)
			}
		} else {
			res.write_u8(`-`)
		}
	}

	/*
	// Legacy version
	// max u64 18446744073709551615 => 20 byte
	mut buf := [32]u8{}
	mut i := 20
	mut d1 := d
	for i >= (21 - n_char) {
		buf[i] = u8(d1 % 10) + `0`
		d1 = d1 / 10
		i--
	}
	i++
	*/

	//===========================================
	// Speed version
	// max u64 18446744073709551615 => 20 byte
	mut buf := [32]u8{}
	mut i := 20
	mut n := d
	mut d_i := u64(0)
	if n > 0 {
		for n > 0 {
			n1 := n / 100
			// calculate the digit_pairs start index
			d_i = (n - (n1 * 100)) << 1
			n = n1
			unsafe {
				buf[i] = digit_pairs.str[d_i]
			}
			i--
			d_i++
			unsafe {
				buf[i] = digit_pairs.str[d_i]
			}
			i--
		}
		i++
		// remove head zero
		if d_i < 20 {
			i++
		}
		unsafe { res.write_ptr(&buf[i], n_char) }
	} else {
		// we have a zero no need of more code!
		res.write_u8(`0`)
	}
	//===========================================

	if p.align == .left {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	return
}

// f64_to_str_lnd1 formats the exact f64 value with dec_digit fractional digits, rounding half up.
pub fn f64_to_str_lnd1(f f64, dec_digit int) string {
	return exact_float_fixed(f, dec_digit)
}

// format_fl formats the exact f64 value in fixed notation according to p, rounding half up.
pub fn format_fl(f f64, p BF_param) string {
	return format_float_magnitude(exact_float_fixed(fabs(f), p.len1), p)
}

// format_es formats the exact f64 value in scientific notation according to p, rounding half up.
pub fn format_es(f f64, p BF_param) string {
	return format_float_magnitude(exact_float_scientific(fabs(f), p.len1), p)
}

// remove_tail_zeros strips trailing zeros from `s` and return the resulting `string`.
@[direct_array_access]
pub fn remove_tail_zeros(s string) string {
	unsafe {
		mut buf := malloc_noscan(s.len + 1)
		mut i_d := 0
		mut i_s := 0

		// skip spaces
		for i_s < s.len && s[i_s] !in [`-`, `+`] && (s[i_s] > `9` || s[i_s] < `0`) {
			buf[i_d] = s[i_s]
			i_s++
			i_d++
		}
		// sign
		if i_s < s.len && s[i_s] in [`-`, `+`] {
			buf[i_d] = s[i_s]
			i_s++
			i_d++
		}

		// integer part
		for i_s < s.len && s[i_s] >= `0` && s[i_s] <= `9` {
			buf[i_d] = s[i_s]
			i_s++
			i_d++
		}

		// check decimals
		if i_s < s.len && s[i_s] == `.` {
			mut i_s1 := i_s + 1
			mut sum := 0
			mut i_s2 := i_s1 // last non-zero index after `.`
			for i_s1 < s.len && s[i_s1] >= `0` && s[i_s1] <= `9` {
				sum += s[i_s1] - u8(`0`)
				if s[i_s1] != `0` {
					i_s2 = i_s1
				}
				i_s1++
			}
			// decimal part must be copied
			if sum > 0 {
				for c_i in i_s .. i_s2 + 1 {
					buf[i_d] = s[c_i]
					i_d++
				}
			}
			i_s = i_s1
		}

		if i_s < s.len && s[i_s] != `.` {
			// check exponent
			for {
				buf[i_d] = s[i_s]
				i_s++
				i_d++
				if i_s >= s.len {
					break
				}
			}
		}

		buf[i_d] = 0
		return tos(buf, i_d)
	}
}
