/*=============================================================================
Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
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

	if p.allign == .right {
		for i1 := 0; i1 < dif; i1++ {
			sb.write_u8(p.pad_ch)
		}
	}
	sb.write_string(s)
	if p.allign == .left {
		for i1 := 0; i1 < dif; i1++ {
			sb.write_u8(p.pad_ch)
		}
	}
}

const (
	max_size_f64_char = 32 // the f64 max representation is -36,028,797,018,963,968e1023, 21 chars, 32 is faster for the memory manger
	// digit pairs in reverse order
	digit_pairs       = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'
)

// format_dec_sb formats an u64 using a `strings.Builder`.
[direct_array_access]
pub fn format_dec_sb(d u64, p BF_param, mut res strings.Builder) {
	mut n_char := dec_digits(d)
	sign_len := if !p.positive || p.sign_flag { 1 } else { 0 }
	number_len := sign_len + n_char
	dif := p.len0 - number_len
	mut sign_written := false

	if p.allign == .right {
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
				buf[i] = strconv.digit_pairs.str[d_i]
			}
			i--
			d_i++
			unsafe {
				buf[i] = strconv.digit_pairs.str[d_i]
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

	if p.allign == .left {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	return
}

// f64_to_str_lnd1 formats a f64 to a `string` with `dec_digit` digits after the dot.
[direct_array_access; manualfree]
pub fn f64_to_str_lnd1(f f64, dec_digit int) string {
	unsafe {
		// we add the rounding value
		s := f64_to_str(f + dec_round[dec_digit], 18)
		// check for +inf -inf Nan
		if s.len > 2 && (s[0] == `n` || s[1] == `i`) {
			return s
		}

		m_sgn_flag := false
		mut sgn := 1
		mut b := [26]u8{}
		mut d_pos := 1
		mut i := 0
		mut i1 := 0
		mut exp := 0
		mut exp_sgn := 1

		mut dot_res_sp := -1

		// get sign and decimal parts
		for c in s {
			match c {
				`-` {
					sgn = -1
					i++
				}
				`+` {
					sgn = 1
					i++
				}
				`0`...`9` {
					b[i1] = c
					i1++
					i++
				}
				`.` {
					if sgn > 0 {
						d_pos = i
					} else {
						d_pos = i - 1
					}
					i++
				}
				`e` {
					i++
					break
				}
				else {
					s.free()
					return '[Float conversion error!!]'
				}
			}
		}
		b[i1] = 0

		// get exponent
		if s[i] == `-` {
			exp_sgn = -1
			i++
		} else if s[i] == `+` {
			exp_sgn = 1
			i++
		}

		mut c := i
		for c < s.len {
			exp = exp * 10 + int(s[c] - `0`)
			c++
		}

		// allocate exp+32 chars for the return string
		// mut res := []u8{len:exp+32,init:`0`}
		mut res := []u8{len: exp + 32, init: 0}
		mut r_i := 0 // result string buffer index

		// println("s:${sgn} b:${b[0]} es:${exp_sgn} exp:${exp}")

		// s no more needed
		s.free()

		if sgn == 1 {
			if m_sgn_flag {
				res[r_i] = `+`
				r_i++
			}
		} else {
			res[r_i] = `-`
			r_i++
		}

		i = 0
		if exp_sgn >= 0 {
			for b[i] != 0 {
				res[r_i] = b[i]
				r_i++
				i++
				if i >= d_pos && exp >= 0 {
					if exp == 0 {
						dot_res_sp = r_i
						res[r_i] = `.`
						r_i++
					}
					exp--
				}
			}
			for exp >= 0 {
				res[r_i] = `0`
				r_i++
				exp--
			}
			// println("exp: $exp $r_i $dot_res_sp")
		} else {
			mut dot_p := true
			for exp > 0 {
				res[r_i] = `0`
				r_i++
				exp--
				if dot_p {
					dot_res_sp = r_i
					res[r_i] = `.`
					r_i++
					dot_p = false
				}
			}
			for b[i] != 0 {
				res[r_i] = b[i]
				r_i++
				i++
			}
		}

		// no more digits needed, stop here
		if dec_digit <= 0 {
			// C.printf(c'f: %f, i: %d, res.data: %p | dot_res_sp: %d | *(res.data): %s \n', f, i, res.data, dot_res_sp, res.data)
			if dot_res_sp < 0 {
				dot_res_sp = i + 1
			}
			tmp_res := tos(res.data, dot_res_sp).clone()
			res.free()
			return tmp_res
		}

		// println("r_i-d_pos: ${r_i - d_pos}")
		if dot_res_sp >= 0 {
			r_i = dot_res_sp + dec_digit + 1
			res[r_i] = 0
			for c1 in 1 .. dec_digit + 1 {
				if res[r_i - c1] == 0 {
					res[r_i - c1] = `0`
				}
			}
			// println("result: [${tos(&res[0],r_i)}]")
			tmp_res := tos(res.data, r_i).clone()
			res.free()
			return tmp_res
		} else {
			if dec_digit > 0 {
				mut c1 := 0
				res[r_i] = `.`
				r_i++
				for c1 < dec_digit {
					res[r_i] = `0`
					r_i++
					c1++
				}
				res[r_i] = 0
			}
			tmp_res := tos(res.data, r_i).clone()
			res.free()
			return tmp_res
		}
	}
}

// format_fl is a `strings.Builder` version of format_fl.
[direct_array_access; manualfree]
pub fn format_fl(f f64, p BF_param) string {
	unsafe {
		mut fs := f64_to_str_lnd1(if f >= 0.0 { f } else { -f }, p.len1)

		// error!!
		if fs[0] == `[` {
			return fs
		}

		if p.rm_tail_zero {
			tmp := fs
			fs = remove_tail_zeros(fs)
			tmp.free()
		}

		mut buf := [strconv.max_size_f64_char]u8{} // write temp float buffer in stack
		mut out := [strconv.max_size_f64_char]u8{} // out buffer
		mut buf_i := 0 // index temporary string
		mut out_i := 0 // index output string

		mut sign_len_diff := 0
		if p.pad_ch == `0` {
			if p.positive {
				if p.sign_flag {
					out[out_i] = `+`
					out_i++
					sign_len_diff = -1
				}
			} else {
				out[out_i] = `-`
				out_i++
				sign_len_diff = -1
			}
		} else {
			if p.positive {
				if p.sign_flag {
					buf[buf_i] = `+`
					buf_i++
				}
			} else {
				buf[buf_i] = `-`
				buf_i++
			}
		}

		// copy the float
		vmemcpy(&buf[buf_i], fs.str, fs.len)
		buf_i += fs.len

		// make the padding if needed
		dif := p.len0 - buf_i + sign_len_diff
		if p.allign == .right {
			for i1 := 0; i1 < dif; i1++ {
				out[out_i] = p.pad_ch
				out_i++
			}
		}
		vmemcpy(&out[out_i], &buf[0], buf_i)
		out_i += buf_i
		if p.allign == .left {
			for i1 := 0; i1 < dif; i1++ {
				out[out_i] = p.pad_ch
				out_i++
			}
		}
		out[out_i] = 0

		// return and free
		tmp := fs
		fs = tos_clone(&out[0])
		tmp.free()
		return fs
	}
}

// format_es returns a f64 as a `string` formatted according to the options set in `p`.
[direct_array_access; manualfree]
pub fn format_es(f f64, p BF_param) string {
	unsafe {
		mut fs := f64_to_str_pad(if f > 0 { f } else { -f }, p.len1)
		if p.rm_tail_zero {
			tmp := fs
			fs = remove_tail_zeros(fs)
			tmp.free()
		}

		mut buf := [strconv.max_size_f64_char]u8{} // write temp float buffer in stack
		mut out := [strconv.max_size_f64_char]u8{} // out buffer
		mut buf_i := 0 // index temporary string
		mut out_i := 0 // index output string

		mut sign_len_diff := 0
		if p.pad_ch == `0` {
			if p.positive {
				if p.sign_flag {
					out[out_i] = `+`
					out_i++
					sign_len_diff = -1
				}
			} else {
				out[out_i] = `-`
				out_i++
				sign_len_diff = -1
			}
		} else {
			if p.positive {
				if p.sign_flag {
					buf[buf_i] = `+`
					buf_i++
				}
			} else {
				buf[buf_i] = `-`
				buf_i++
			}
		}

		// copy the float
		vmemcpy(&buf[buf_i], fs.str, fs.len)
		buf_i += fs.len

		// make the padding if needed
		dif := p.len0 - buf_i + sign_len_diff
		if p.allign == .right {
			for i1 := 0; i1 < dif; i1++ {
				out[out_i] = p.pad_ch
				out_i++
			}
		}
		vmemcpy(&out[out_i], &buf[0], buf_i)
		out_i += buf_i
		if p.allign == .left {
			for i1 := 0; i1 < dif; i1++ {
				out[out_i] = p.pad_ch
				out_i++
			}
		}
		out[out_i] = 0

		// return and free
		tmp := fs
		fs = tos_clone(&out[0])
		tmp.free()
		return fs
	}
}

// remove_tail_zeros strips traling zeros from `s` and return the resulting `string`.
[direct_array_access]
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
			for i_s1 < s.len && s[i_s1] >= `0` && s[i_s1] <= `9` {
				sum += s[i_s1] - u8(`0`)
				i_s1++
			}
			// decimal part must be copied
			if sum > 0 {
				for c_i in i_s .. i_s1 {
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
