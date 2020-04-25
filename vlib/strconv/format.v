/**********************************************************************
*
* printf/sprintf V implementation
*
* Copyright (c) 2020 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* This file contains the printf/sprintf functions
*
* TODO: th_separator, g for float, e,f uppercase for float
**********************************************************************/
module strconv
import strconv.ftoa
import strings
import math

enum Char_parse_state {
	start,
	norm_char,
	field_char,
	pad_ch,
	len_set_start,
	len_set_in,

	check_type,
	check_float,
	check_float_in,

	reset_params

}

enum Align_text {
	right = 0,
	left,
	center
}

/******************************************************************************
*
* Float conversion utility
*
******************************************************************************/
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
	s := ftoa.f64_to_str(f + dec_round[dec_digit], 18)
	// check for +inf -inf Nan
	if s.len > 2 && (s[0] == `n` || s[1] == `i`) {
		return s
	}

	m_sgn_flag := false
	mut sgn        := 1
	mut b          := [26]byte 
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
	mut res := [`0`].repeat(exp+32) // TODO: Slow!! is there other possibilities to allocate this?
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
		return tos(&res[0],r_i)
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
		return tos(&res[0],r_i)
	}
}

/******************************************************************************
*
* Write to string builder functions
*
******************************************************************************/
struct BF_param {
	pad_ch       byte       = ` `
	len0         int        = -1
	len1         int        = 6
	positive     bool       = true      
	sign_flag    bool       = false
	allign       Align_text = .right
	rm_tail_zero bool       = false    // remove the tails zeros from floats
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
	res.write(s)
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
	
	res.write(s)
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
	
	res.write(s)
	if p.allign == .left {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)						
		}
	}

	return res.str()
}

pub fn format_es(f f64, p BF_param) string {
	mut s := ""
	mut fs := ftoa.f64_to_str_pad(if f> 0 {f} else {-f},p.len1)
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
	res.write(s)
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
		ch := s.str[i]
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
			tmp = s[..dot_pos] + s [i..]
		}else {
			tmp = s[..last_zero_start] + s [i..]
		}
	} else {
		tmp = s
	}
	if tmp.str[tmp.len-1] == `.` {
		return tmp[..tmp.len-1]
	}
	
	return tmp
}

/******************************************************************************
*
* Main functions
*
******************************************************************************/
pub fn v_printf(str string, pt ... voidptr) {
	print(v_sprintf(str, pt))
}

pub fn v_sprintf(str string, pt ... voidptr) string{

	mut res := strings.new_builder(pt.len * 16)

	mut i            := 0                // main strign index
	mut p_index      := 0                // parameter index
	mut sign         := false            // sign flag
	mut allign       := Align_text.right
	mut len0         := -1               // forced length, if -1 free length
	mut len1         := -1               // decimal part for floats
	def_len1         := 6                // default value for len1
	mut pad_ch       := ` `              // pad char
	mut th_separator := false            // thousands separator flag

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
			th_separator = false
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
			d1 := *(&byte(pt[p_index]))
			res.write_b(d1)
			status = .reset_params
			p_index++
			i++
			continue
		}

		// pointer, manage it here
		if ch == `p` && status == .field_char {
			res.write("0x"+ptr_str(pt[p_index]))
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
				th_separator = true
				i++
				continue
			} else if ch == `.` && fc_ch1 >= `1` && fc_ch1 <= `9` {
				status = .check_float
				i++
				continue
			} 
			// manage "%.*s" precision field
			else if ch == `.` && fc_ch1 == `*` && fc_ch2 == `s` {
				len := *(&int(pt[p_index]))
				p_index++
				mut s := *(&string(pt[p_index]))
				s = s[..len]
				p_index++
				res.write(s)				

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
						//i++
						if ch2 == `h` {
							//i++
							x := *(&i8(pt[p_index]))
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						} else {
							x := *(&i16(pt[p_index]))
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						}
					}
					// l  i64
					// ll i64 for now
					`l` {
						//i++
						if ch2 == `l` {
							//i++
							x := *(&i64(pt[p_index]))
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						} else {
							x := *(&i64(pt[p_index]))
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						}
					}
					// defualt int
					else {
						x := *(&int(pt[p_index]))
						positive = if x >= 0 { true } else { false }
						d1 = if positive { u64(x) } else { u64(-x) }
					}

				}

				res.write(format_dec(d1,{positive: positive, pad_ch: pad_ch, len0: len0, sign_flag: sign, allign: allign}))
				
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

				match ch1 {
					// h for 16 bit unsigned int
					// hh fot 8 bit unsigned int
					`h` {
						//i++
						if ch2 == `h` {
							//i++
							d1 = u64(*(&byte(pt[p_index])))
						} else {
							d1 = u64(*(&u16(pt[p_index])))
						}
					}
					// l  u64
					// ll u64 for now
					`l` {
						//i++
						if ch2 == `l` {
							//i++
							d1 = u64(*(&u64(pt[p_index])))
						} else {
							d1 = u64(*(&u64(pt[p_index])))
						}
					}
					// defualt int
					else {
						d1 = u64(*(&u32(pt[p_index])))
					}
				}

				res.write(format_dec(d1,{positive: positive, pad_ch: pad_ch, len0: len0, sign_flag: sign, allign: allign}))
				status = .reset_params
				p_index++
				i++
				continue
			}

			// hex
			else if ch in [`x`, `X`] {
				mut s := ""

				match ch1 {
					// h for 16 bit int
					// hh fot 8 bit int
					`h` {
						//i++
						if ch2 == `h` {
							//i++
							x := *(&i8(pt[p_index]))
							s = x.hex()
						} else {
							x := *(&i16(pt[p_index]))
							s = x.hex()
						}
					}
					// l  i64
					// ll i64 for now
					`l` {
						//i++
						if ch2 == `l` {
							// i++
							x := *(&i64(pt[p_index]))
							s = x.hex()
						} else {
							x := *(&i64(pt[p_index]))
							s = x.hex()
						}
					} 
					else {
						x := *(&int(pt[p_index]))
						s = x.hex()
					}
				}

				if ch == `X` {
					s = s.to_upper()
				}

				res.write(format_str(s,{pad_ch: pad_ch, len0: len0, allign: allign}))
				status = .reset_params
				p_index++
				i++
				continue
			}

			// float and double
			if ch in [`f`, `F`] {
				x := *(&f64(pt[p_index]))
				mut positive := x >= f64(0.0)
				len1 = if len1 >= 0 { len1 } else { def_len1 }
				s := format_fl(f64(x), {positive: positive, pad_ch: pad_ch, len0: len0, len1: len1, sign_flag: sign, allign: allign})
				res.write(if ch == `F` {s.to_upper()} else {s})
				status = .reset_params
				p_index++
				i++
				continue
			}
			else if ch in [`e`, `E`] {
				x := *(&f64(pt[p_index]))
				mut positive := x >= f64(0.0)
				len1 = if len1 >= 0 { len1 } else { def_len1 }
				s := format_es(f64(x), {positive: positive, pad_ch: pad_ch, len0: len0, len1: len1, sign_flag: sign, allign: allign})
				res.write(if ch == `E` {s.to_upper()} else {s})
				status = .reset_params
				p_index++
				i++
				continue
			}
			else if ch in [`g`, `G`] {
				x := *(&f64(pt[p_index]))
				mut positive := x >= f64(0.0)
				mut s := ""
				tx := math.abs(x)
				if tx < 999_999.0 && tx >= 0.00001 {
					//println("Here g format_fl [$tx]")
					len1 = if len1 >= 0 { len1+1 } else { def_len1 }
					s = format_fl(x, {positive: positive, pad_ch: pad_ch, len0: len0, len1: len1, sign_flag: sign, allign: allign, rm_tail_zero: true})
				} else {
					len1 = if len1 >= 0 { len1+1 } else { def_len1 }
					s = format_es(x, {positive: positive, pad_ch: pad_ch, len0: len0, len1: len1, sign_flag: sign, allign: allign, rm_tail_zero: true})
				}
				res.write(if ch == `G` {s.to_upper()} else {s})
				status = .reset_params
				p_index++
				i++
				continue
			}

			// string
			else if ch == `s` {
				s1 := *(&string(pt[p_index]))
				pad_ch = ` ` // for now we use only space
				res.write(format_str(s1, {pad_ch: pad_ch, len0: len0, allign: allign}))
				
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

	return res.str()
	//print(res.str())
}




fn main(){
/*
	x := f64(0.1234567890e3)
	//println("${ftoa.ftoa_64(x)}")
	//println("f : ${ftoa.f64_to_str_lnd(x,5)}")
	//println("fl: ${ftoa.ftoa_long_64(x)}")
	//println("fd:${ftoa.f64_to_str_l(x)}")
	cs := "[%40.5f]\n"
	v_printf(cs,x)
	C.printf(cs.str,x)
*/

	mut buf    := [1024]byte
	mut temp_s := ""
	a0  := u32(10)
	b0  := 200
	c0  := byte(12)
	s0  := "ciAo"
	ch0 := `B`
	
	f0  := 0.312345
	f1  := 200000.0
	f2  := -1234.300e6
	f3  := 1234.300e-6
	
	sc0 := "ciao: [%-08u] %d %hhd [%08s]\nr2: [%08X] [%p] [%-20.4f] [%-20.4f] [%c]\n"
	temp_s = v_sprintf(sc0    ,a0 ,b0 ,c0 ,s0     ,b0 ,&b0 ,f0, f1, ch0)
	C.sprintf(buf, sc0.str,a0 ,b0 ,c0 ,s0.str ,b0 ,&b0 ,f0, f1, ch0)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	a := byte(12)
	b := i16(13)
	c := 14
	d := i64(15)
	sc1 := "==>%hhd %hd %d %ld\n"
	temp_s = v_sprintf(sc1, a ,b ,c, d)
	C.sprintf(buf, sc1.str, a ,b ,c, d)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	a1 := byte(0xff)
	b1 := i16(0xffff)
	c1 := u32(0xffff_ffff)
	d1 := u64(-1)
	sc2 := "%hhu %hu %u %lu\n"
	temp_s = v_sprintf(sc2, a1 ,b1 ,c1, d1)
	C.sprintf(buf, sc2.str, a1 ,b1 ,c1, d1)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s
	

	sc3 := "%hhx %hx %x %lx\n"
	temp_s = v_sprintf(sc3, a1 ,b1 ,c1, d1)
	C.sprintf(buf, sc3.str, a1 ,b1 ,c1, d1)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s
	

	sc4 := "[%-20.3e] [%20.3e] [%-020.3e] [%-020.3E] [%-020.3e] [%-020.3e]\n"
	temp_s = v_sprintf(sc4, f0, f1, f1, f1, f2, f3)
	C.sprintf(buf, sc4.str, f0, f1, f1, f1, f2, f3)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s
	
	sc5 := "[%.3f] [%0.3f] [%0.3F] [%0.3f] [%0.3F]\n"
	temp_s = v_sprintf(sc5, f0, f1, f1, f2, f3, f3)
	C.sprintf(buf, sc5.str, f0, f1, f1, f2, f3, f3)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	ml  := 3
	sc6 := "%.*s [%05hhX]\n"
	temp_s = v_sprintf(sc6, ml, s0    , a)
	C.sprintf(buf, sc6.str, ml, s0.str, a)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	a2 := 125
	sc7 := "[%9x] [%9X] [%-9x] [%-9X] [%09x] [%09X]\n"
	temp_s = v_sprintf(sc7, a2, a2, a2, a2, a2, a2)
	C.sprintf(buf, sc7.str, a2, a2, a2, a2, a2, a2)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	mut ft := -1e-7
	mut x  := 0
	sc8    := "[%20g][%20G]|"
	for x < 12 {
		temp_s = v_sprintf(sc8, ft, ft)
		C.sprintf(buf,sc8.str, ft, ft)
		//println("$temp_s ${tos2(buf)}")
		assert tos2(buf) == temp_s
		ft = ft * 10.0
		x++
	}

	mut tm := -1e-6
	println("($tm)")
	tm = tm * 10.0
	sc9 := "[%20g]\n"
	temp_s = v_sprintf(sc9, tm)
	print(temp_s)
	C.printf(sc9.str, tm)
	
}

/*
//	a := sh1_str_to_byte_array("de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3")

fn sh1_str_to_byte_array(in_str string) []byte{
	mut b := []byte
	mut bt := byte(0)
	for c,i in in_str {
		if c & 1 == 1 {
			bt = bt << 4
		} else {
			bt = 0
		}

		if i >= `a` {
			bt += i - `a` + 10
		} else {
			bt += i - `0`
		}	
		
		if c & 1 == 1 {
			b << bt
			//b[c>>1] = bt
			//print("$c ${bt.hex()}|")
		}
		
	}
	return b
}
*/