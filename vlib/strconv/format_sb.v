/*=============================================================================
Copyright (c) 2019-2021 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
=============================================================================*/
module strconv
import strings

// builder version of format_str
pub fn format_str_sb(s string, p BF_param, mut sb strings.Builder) {
	if p.len0 <= 0 {
		//return s.clone()
		sb.write_string(s)
		return
	}
	dif := p.len0 - utf8_str_visible_length(s)
	if dif <= 0 {
		sb.write_string(s)
		return
	}
	
	if p.allign == .right {
		for i1 :=0; i1 < dif; i1++ {
			sb.write_b(p.pad_ch)
		}
	}
	sb.write_string(s)
	if p.allign == .left {
		for i1 :=0; i1 < dif; i1++ {
			sb.write_b(p.pad_ch)
		}
	}
}


// string builder version of format_dec
[manualfree]
pub fn format_dec_sb(d u64, p BF_param, mut res strings.Builder) {
	mut s := ""
	mut sign_len_diff := 0
	if p.pad_ch == `0` {
		if p.positive {
			if p.sign_flag {
				res.write_b(`+`)
				sign_len_diff = -1
			}
		} else {
			res.write_b(`-`)
			sign_len_diff = -1
		}
		s = d.str()
	} else {
		if p.positive {
			if p.sign_flag {
				s = "+" + d.str()
			} else {
				s = d.str()
			}
		} else {
			s = "-" + d.str()
		}
	}
	dif := p.len0 - s.len + sign_len_diff

	if p.allign == .right {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	}
	res.write_string(s)
	unsafe{ s.free() }
	if p.allign == .left {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	}
}

//=============================================================================
// Int to strings using strings.Builder
//=============================================================================
//const base_digits = '0123456789abcdefghijklmnopqrstuvwxyz'

// format_int returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
pub fn format_int_sb(n i64, radix int) string {
	if radix < 2 || radix > 36 {
		panic('invalid radix: $radix . It should be => 2 and <= 36')
	}
	if n == 0 {
		return '0'
	}
	mut n_copy := n
	mut sign := ''
	if n < 0 {
		sign = '-'
		n_copy = -n_copy
	}
	mut res := ''
	for n_copy != 0 {
		res = base_digits[n_copy % radix].ascii_str() + res
		n_copy /= radix
	}
	return '$sign$res'
}
/*
// format_uint returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
[manualfree]
pub fn format_uint_sb(n u64, radix int, mut sb strings.Builder) {
	unsafe{ 
		if radix < 2 || radix > 36 {
			panic('invalid radix: $radix . It should be => 2 and <= 36')
		}
		if n == 0 {
			sb.write_string('0')
		}
		mut n_copy := n
		mut res := ''
		uradix := u64(radix)
		for n_copy != 0 {
			tmp_0 := res
			tmp1  :=  base_digits[n_copy % uradix].ascii_str()
			res = tmp1 + res
			tmp_0.free()
			tmp_1.free()
			n_copy /= uradix
		}
		sb.write_string(res)
		res.free()
	}
}
*/