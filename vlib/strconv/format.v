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

enum Align_text {
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
			b[i1++] = c
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
			res[r_i++] = `+`
		}
	} else {
		res[r_i++] = `-`
	}

	i = 0
	if exp_sgn >= 0 {
		for b[i] != 0 {
			res[r_i++] = b[i]
			i++
			if i >= d_pos && exp >= 0 {
				if exp == 0 {
					dot_res_sp = r_i
					res[r_i++] = `.`
				}
				exp--
			}
		}
		for exp >= 0 {
			res[r_i++] = `0`
			exp--
		}
		//println("exp: $exp $r_i $dot_res_sp")
	} else {
		mut dot_p := true
		for exp > 0 {
			res[r_i++] = `0`
			exp--
			if dot_p  {
				dot_res_sp = r_i
				res[r_i++] = `.`
				dot_p = false
			}
		}
		for b[i] != 0 {
			res[r_i++] = b[i]
			i++
		}
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
			res[r_i++] = `.`
			for c < dec_digit {
				res[r_i++] = `0`
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
	pad_ch       byte       = byte(` `)     // padding char
	len0         int        = -1      // default len for whole the number or string
	len1         int        = 6       // number of decimal digits, if needed
	positive     bool       = true    // mandatory: the sign of the number passed
	sign_flag    bool                 // flag for print sign as prefix in padding
	allign       Align_text = .right  // alignment of the string
	rm_tail_zero bool                 // remove the tail zeros from floats
}

pub fn format_str(s string, p BF_param) string {
	dif := p.len0 - s.len
	if dif <= 0 {
		return s
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

pub fn format_fl(f f64, p BF_param) string {
	mut s  := ""
	mut fs := f64_to_str_lnd(if f >= 0.0 {f} else {-f}, p.len1)

	// error!!
	if fs[0] == `[` {
		return fs
	}

	if p.rm_tail_zero {
		fs = remove_tail_zeros(fs)
	}
	mut res := strings.new_builder( if p.len0 > fs.len { p.len0 } else { fs.len })

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
		s = fs
	} else {
		if p.positive {
			if p.sign_flag {
				s = "+" + fs
			} else {
				s = fs
			}
		} else {
			s = "-" + fs
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

pub fn format_es(f f64, p BF_param) string {
	mut s := ""
	mut fs := f64_to_str_pad(if f> 0 {f} else {-f},p.len1)
	if p.rm_tail_zero {
		fs = remove_tail_zeros(fs)
	}
	mut res := strings.new_builder( if p.len0 > fs.len { p.len0 } else { fs.len })

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
		s = fs
	} else {
		if p.positive {
			if p.sign_flag {
				s = "+" + fs
			} else {
				s = fs
			}
		} else {
			s = "-" + fs
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

/*

Main functions

*/
pub fn v_printf(str string, pt ... voidptr) {
	print(v_sprintf(str, pt))
}

pub fn v_sprintf(str string, pt ... voidptr) string{
	mut res := strings.new_builder(pt.len * 16)

	mut i            := 0                // main string index
	mut p_index      := 0                // parameter index
	mut sign         := false            // sign flag
	mut allign       := Align_text.right
	mut len0         := -1               // forced length, if -1 free length
	mut len1         := -1               // decimal part for floats
	def_len1         := 6                // default value for len1
	mut pad_ch       := byte(` `)        // pad char

	// prefix chars for Length field
	mut ch1 := `0`  // +1 char if present else `0`
	mut ch2 := `0`  // +2 char if present else `0`

	mut status := Char_parse_state.norm_char
	for i < str.len {
		if status == .reset_params {
			sign         = false
			allign       = .right
			len0         = -1
			len1         = -1
			pad_ch       = ` `
			status = .norm_char
			ch1 = `0`
			ch2 = `0`
			continue
		}

		ch := str[i]
		if ch != `%` && status == .norm_char {
			res.write_b(ch)
			i++
			continue
		}
		if ch == `%` && status == .norm_char {
			status = .field_char
			i++
			continue
		}

		// single char, manage it here
		if ch == `c` && status == .field_char {
			v_sprintf_panic(p_index, pt.len)
			d1 := unsafe {*(&byte(pt[p_index]))}
			res.write_b(d1)
			status = .reset_params
			p_index++
			i++
			continue
		}

		// pointer, manage it here
		if ch == `p` && status == .field_char {
			v_sprintf_panic(p_index, pt.len)
			res.write_string("0x")
			res.write_string(ptr_str(unsafe {pt[p_index]}))
			status = .reset_params
			p_index++
			i++
			continue
		}

		if status == .field_char {
			mut fc_ch1 := `0`
			mut fc_ch2 := `0`
			if (i + 1) < str.len {
				fc_ch1 = str[i+1]
				if (i + 2) < str.len {
					fc_ch2 = str[i+2]
				}
			}
			if ch == `+` {
				sign = true
				i++
				continue
			} else if ch == `-` {
				allign = .left
				i++
				continue
			} else if ch in [`0`,` `] {
				if allign == .right {
					pad_ch = ch
				}
				i++
				continue
			} else if ch == `'` {
				i++
				continue
			} else if ch == `.` && fc_ch1 >= `1` && fc_ch1 <= `9` {
				status = .check_float
				i++
				continue
			}
			// manage "%.*s" precision field
			else if ch == `.` && fc_ch1 == `*` && fc_ch2 == `s` {
				v_sprintf_panic(p_index, pt.len)
				len := unsafe {*(&int(pt[p_index]))}
				p_index++
				v_sprintf_panic(p_index, pt.len)
				mut s := unsafe {*(&string(pt[p_index]))}
				s = s[..len]
				p_index++
				res.write_string(s)
				status = .reset_params
				i += 3
				continue
			}
			status = .len_set_start
			continue
		}

		if status == .len_set_start {
			if ch >= `1` && ch <= `9` {
				len0 = int(ch - `0`)
				status = .len_set_in
				i++
				continue
			}
			if ch == `.` {
				status = .check_float
				i++
				continue
			}
			status = .check_type
			continue
		}

		if status == .len_set_in {
			if ch >= `0` && ch <= `9` {
				len0 *= 10
				len0 += int(ch - `0`)
				i++
				continue
			}
			if ch == `.` {
				status = .check_float
				i++
				continue
			}
			status = .check_type
			continue
		}

		if status == .check_float {
			if ch >= `0` && ch <= `9` {
				len1 = int(ch - `0`)
				status = .check_float_in
				i++
				continue
			}
			status = .check_type
			continue
		}

		if status == .check_float_in {
			if ch >= `0` && ch <= `9` {
				len1 *= 10
				len1 += int(ch - `0`)
				i++
				continue
			}
			status = .check_type
			continue
		}

		if status == .check_type {

			if ch == `l` {
				if ch1 == `0` {
					ch1 = `l`
					i++
					continue
				} else {
					ch2 = `l`
					i++
					continue
				}
			}
			else if ch == `h` {
				if ch1 == `0` {
					ch1 = `h`
					i++
					continue
				} else {
					ch2 = `h`
					i++
					continue
				}
			}

			// signed integer
			else if ch in [`d`,`i`] {
				mut d1 := u64(0)
				mut positive := true

				//println("$ch1 $ch2")
				match ch1 {
					// h for 16 bit int
					// hh fot 8 bit int
					`h` {
						if ch2 == `h` {
							v_sprintf_panic(p_index, pt.len)
							x := unsafe {*(&i8(pt[p_index]))}
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						} else {
							x := unsafe {*(&i16(pt[p_index]))}
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						}
					}
					// l  i64
					// ll i64 for now
					`l` {
						// placeholder for future 128bit integer code
						/*
						if ch2 == `l` {
							v_sprintf_panic(p_index, pt.len)
							x := *(&i128(pt[p_index]))
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u128(x) } else { u128(-x) }
						} else {
							v_sprintf_panic(p_index, pt.len)
							x := *(&i64(pt[p_index]))
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						}
						*/
						v_sprintf_panic(p_index, pt.len)
						x := unsafe {*(&i64(pt[p_index]))}
						positive = if x >= 0 { true } else { false }
						d1 = if positive { u64(x) } else { u64(-x) }
					}
					// default int
					else {
						v_sprintf_panic(p_index, pt.len)
						x := unsafe {*(&int(pt[p_index]))}
						positive = if x >= 0 { true } else { false }
						d1 = if positive { u64(x) } else { u64(-x) }
					}

				}
				res.write_string(format_dec(d1,{pad_ch: pad_ch, len0: len0, len1: 0, positive: positive, sign_flag: sign, allign: allign}))
				status = .reset_params
				p_index++
				i++
				ch1 = `0`
				ch2 = `0`
				continue
			}

			// unsigned integer
			else if ch == `u` {
				mut d1 := u64(0)
				positive := true
				v_sprintf_panic(p_index, pt.len)
				match ch1 {
					// h for 16 bit unsigned int
					// hh fot 8 bit unsigned int
					`h` {
						if ch2 == `h` {
							d1 = u64(unsafe {*(&byte(pt[p_index]))})
						} else {
							d1 = u64(unsafe {*(&u16(pt[p_index]))})
						}
					}
					// l  u64
					// ll u64 for now
					`l` {
						// placeholder for future 128bit integer code
						/*
						if ch2 == `l` {
							d1 = u128(*(&u128(pt[p_index])))
						} else {
							d1 = u64(*(&u64(pt[p_index])))
						}
						*/
						d1 = u64(unsafe {*(&u64(pt[p_index]))})
					}
					// default int
					else {
						d1 = u64(unsafe {*(&u32(pt[p_index]))})
					}
				}

				res.write_string(format_dec(d1,{pad_ch: pad_ch, len0: len0, len1: 0, positive: positive, sign_flag: sign, allign: allign}))
				status = .reset_params
				p_index++
				i++
				continue
			}

			// hex
			else if ch in [`x`, `X`] {
				v_sprintf_panic(p_index, pt.len)
				mut s := ""
				match ch1 {
					// h for 16 bit int
					// hh fot 8 bit int
					`h` {
						if ch2 == `h` {
							x := unsafe {*(&i8(pt[p_index]))}
							s = x.hex()
						} else {
							x := unsafe {*(&i16(pt[p_index]))}
							s = x.hex()
						}
					}
					// l  i64
					// ll i64 for now
					`l` {
						// placeholder for future 128bit integer code
						/*
						if ch2 == `l` {
							x := *(&i128(pt[p_index]))
							s = x.hex()
						} else {
							x := *(&i64(pt[p_index]))
							s = x.hex()
						}
						*/
						x := unsafe {*(&i64(pt[p_index]))}
						s = x.hex()
					}
					else {
						x := unsafe {*(&int(pt[p_index]))}
						s = x.hex()
					}
				}

				if ch == `X` {
					s = s.to_upper()
				}

				res.write_string(format_str(s,{pad_ch: pad_ch, len0: len0, len1: 0, positive: true, sign_flag: false, allign: allign}))
				status = .reset_params
				p_index++
				i++
				continue
			}

			// float and double
			if ch in [`f`, `F`] {
				v_sprintf_panic(p_index, pt.len)
				x := unsafe {*(&f64(pt[p_index]))}
				positive := x >= f64(0.0)
				len1 = if len1 >= 0 { len1 } else { def_len1 }
				s := format_fl(f64(x), {pad_ch: pad_ch, len0: len0, len1: len1, positive: positive, sign_flag: sign, allign: allign})
				res.write_string(if ch == `F` {s.to_upper()} else {s})
				status = .reset_params
				p_index++
				i++
				continue
			}
			else if ch in [`e`, `E`] {
				v_sprintf_panic(p_index, pt.len)
				x := unsafe {*(&f64(pt[p_index]))}
				positive := x >= f64(0.0)
				len1 = if len1 >= 0 { len1 } else { def_len1 }
				s := format_es(f64(x), {pad_ch: pad_ch, len0: len0, len1: len1, positive: positive, sign_flag: sign, allign: allign})
				res.write_string(if ch == `E` {s.to_upper()} else {s})
				status = .reset_params
				p_index++
				i++
				continue
			}
			else if ch in [`g`, `G`] {
				v_sprintf_panic(p_index, pt.len)
				x := unsafe {*(&f64(pt[p_index]))}
				positive := x >= f64(0.0)
				mut s := ""
				tx := fabs(x)
				if tx < 999_999.0 && tx >= 0.00001 {
					//println("Here g format_fl [$tx]")
					len1 = if len1 >= 0 { len1+1 } else { def_len1 }
					s = format_fl(x, {pad_ch: pad_ch, len0: len0, len1: len1, positive: positive, sign_flag: sign, allign: allign, rm_tail_zero: true})
				} else {
					len1 = if len1 >= 0 { len1+1 } else { def_len1 }
					s = format_es(x, {pad_ch: pad_ch, len0: len0, len1: len1, positive: positive, sign_flag: sign, allign: allign, rm_tail_zero: true})
				}
				res.write_string(if ch == `G` {s.to_upper()} else {s})
				status = .reset_params
				p_index++
				i++
				continue
			}

			// string
			else if ch == `s` {
				v_sprintf_panic(p_index, pt.len)
				s1 := unsafe{*(&string(pt[p_index]))}
				pad_ch = ` `
				res.write_string(format_str(s1, {pad_ch: pad_ch, len0: len0, len1: 0, positive: true, sign_flag: false, allign: allign}))
				status = .reset_params
				p_index++
				i++
				continue
			}
		}

		status = .reset_params
		p_index++
		i++
	}

	if p_index != pt.len {
		panic('${p_index} % conversion specifiers, but given ${pt.len} args')
	}

	return res.str()
}

[inline]
fn v_sprintf_panic(idx int, len int) {
	if idx >= len {
		panic('${idx+1} % conversion specifiers, but given only ${len} args')
	}
}

fn fabs(x f64) f64 {
	if x < 0.0 {
		return -x
	}
	return x
}
