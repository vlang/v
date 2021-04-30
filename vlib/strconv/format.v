module strconv

/*

printf/sprintf V implementation

Copyright (c) 2020 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the printf/sprintf functions

*/

import strings

enum Char_parse_state {
	start
	norm_char
	field_char
	pad_ch
	len_set_start
	len_set_in

	check_type
	check_float
	check_float_in

	reset_params
}

pub enum Align_text {
	right = 0
	left
	center
}

/*

Float conversion utility

*/
const(
	// rounding value
	dec_round = [
		f64(0.44),
		0.044,
		0.0044,
		0.00044,
		0.000044,
		0.0000044,
		0.00000044,
		0.000000044,
		0.0000000044,
		0.00000000044,
		0.000000000044,
		0.0000000000044,
		0.00000000000044,
		0.000000000000044,
		0.0000000000000044,
		0.00000000000000044,
		0.000000000000000044,
		0.0000000000000000044,
		0.00000000000000000044,
		0.000000000000000000044,
	]
)

// max float 1.797693134862315708145274237317043567981e+308

pub fn f64_to_str_lnd(f f64, dec_digit int) string {
	// we add the rounding value
	s := f64_to_str(f + dec_round[dec_digit], 18)
	// check for +inf -inf Nan
	if s.len > 2 && (s[0] == `n` || s[1] == `i`) {
		return s
	}

	m_sgn_flag := false
	mut sgn        := 1
	mut b          := [26]byte{}
	mut d_pos      := 1
	mut i          := 0
	mut i1         := 0
	mut exp        := 0
	mut exp_sgn    := 1

	mut dot_res_sp := -1

	// get sign and deciaml parts
	for c in s {
		if c == `-` {
			sgn = -1
			i++
		} else if c == `+` {
			sgn = 1
			i++
		}
		else if c >= `0` && c <= `9` {
			b[i1] = c
			i1++
			i++
		} else if c == `.` {
			if sgn > 0 {
				d_pos = i
			} else {
				d_pos = i-1
			}
			i++
		} else if c == `e` {
			i++
			break
		} else {
			return "[Float conversion error!!]"
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
	for c in s[i..] {
		exp = exp * 10 + int(c-`0`)
	}

	// allocate exp+32 chars for the return string
	//mut res := []byte{len:exp+32,init:`0`}
	mut res := []byte{len: exp+32, init: 0}
	mut r_i := 0  // result string buffer index

	//println("s:${sgn} b:${b[0]} es:${exp_sgn} exp:${exp}")

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
		//println("exp: $exp $r_i $dot_res_sp")
	} else {
		mut dot_p := true
		for exp > 0 {
			res[r_i] = `0`
			r_i++
			exp--
			if dot_p  {
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
		return unsafe { tos(res.data, dot_res_sp) }
	}

	//println("r_i-d_pos: ${r_i - d_pos}")
	if dot_res_sp >= 0 {
		if (r_i - dot_res_sp) > dec_digit {
			r_i = dot_res_sp + dec_digit + 1
		}
		res[r_i] = 0
		//println("result: [${tos(&res[0],r_i)}]")
		return unsafe { tos(res.data, r_i) }
	} else {
		if dec_digit > 0 {
			mut c := 0
			res[r_i] = `.`
			r_i++
			for c < dec_digit {
				res[r_i] = `0`
				r_i++
				c++
			}
			res[r_i] = 0
		}
		return unsafe { tos(res.data, r_i) }
	}
}

/*

 Single format functions

*/
pub struct BF_param {
pub mut:
	pad_ch       byte       = byte(` `)     // padding char
	len0         int        = -1      // default len for whole the number or string
	len1         int        = 6       // number of decimal digits, if needed
	positive     bool       = true    // mandatory: the sign of the number passed
	sign_flag    bool                 // flag for print sign as prefix in padding
	allign       Align_text = .right  // alignment of the string
	rm_tail_zero bool                 // remove the tail zeros from floats
}

pub fn format_str(s string, p BF_param) string {
	if p.len0 <= 0 {
		return s.clone()
	}
	dif := p.len0 - utf8_str_visible_length(s)
	if dif <= 0 {
		return s.clone()
	}
	mut res := strings.new_builder(s.len + dif)
	if p.allign == .right {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	}
	res.write_string(s)
	if p.allign == .left {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	}
	return res.str()
}

// max int64 9223372036854775807
pub fn format_dec(d u64, p BF_param) string {
	mut s := ""
	mut res := strings.new_builder(20)
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
	if p.allign == .left {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	}
	return res.str()
}


pub fn remove_tail_zeros(s string) string {
	mut i := 0
	mut last_zero_start := -1
	mut dot_pos         := -1
	mut in_decimal := false
	mut prev_ch := byte(0)
	for i < s.len {
		ch := unsafe {s.str[i]}
		if ch == `.` {
			in_decimal = true
			dot_pos = i
		}
		else if in_decimal {
			if ch == `0` && prev_ch != `0` {
				last_zero_start = i
			} else if ch >= `1` && ch <= `9` {
				last_zero_start = -1
			} else if ch == `e` {
				break
			}
		}
		prev_ch = ch
		i++
	}

	mut tmp := ""
	if last_zero_start > 0 {
		if last_zero_start == dot_pos+1 {
			tmp = s[..dot_pos] + s[i..]
		}else {
			tmp = s[..last_zero_start] + s[i..]
		}
	} else {
		tmp = s
	}
	if unsafe {tmp.str[tmp.len-1]} == `.` {
		return tmp[..tmp.len-1]
	}
	return tmp
}


