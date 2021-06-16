/*=============================================================================
Copyright (c) 2019-2021 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
=============================================================================*/
module strconv
import strings

// strings.Builder version of format_str
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

const (
	// digit pairs in reverse order
	digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'
)

// format_dec_sb format a u64
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
					res.write_b(`+`)
					sign_written = true
				}
			} else {
				res.write_b(`-`)
				sign_written = true
			}
		}
		// write the pad chars
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	} 

	if !sign_written {
		// no pad char, write the sign before the number
		if p.positive {
			if p.sign_flag {
				res.write_b(`+`)
			}
		} else {
			res.write_b(`-`)
		}
	}

	/*
	// Legacy version
	// max u64 18446744073709551615 => 20 byte
	mut buf := [32]byte{}
	mut i := 20
	mut d1 := d
	for i >= (21 - n_char) {
		buf[i] = byte(d1 % 10) + `0`
		d1 = d1 / 10
		i--
	}
	i++
	*/

	//===========================================
	// Speed version
	// max u64 18446744073709551615 => 20 byte
	mut buf := [32]byte{}
	mut i := 20
	mut n := d
	mut d_i := u64(0)
	if n > 0 {
		for n > 0 {
			n1 := n / 100
			// calculate the digit_pairs start index
			d_i = (n - (n1 * 100)) << 1
			n = n1
			unsafe{ buf[i] = digit_pairs.str[d_i] }
			i--
			d_i++
			unsafe{ buf[i] = digit_pairs.str[d_i] }
			i--
		}
		i++
		// remove head zero
		if d_i < 20 {
			i++
		}
		unsafe{ res.write_ptr(&buf[i],n_char) }

	} else {
		// we have a zero no need of more code!
		res.write_b(`0`)
	}
	//===========================================

	if p.allign == .left {
		for i1 :=0; i1 < dif; i1++ {
			res.write_b(p.pad_ch)
		}
	}
	return
}



[manualfree]
[direct_array_access]
pub fn f64_to_str_lnd1(f f64, dec_digit int) string {
	unsafe{
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
				s.free()
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

		mut c := i
		for c < s.len {
			exp = exp * 10 + int(s[c] - `0`)
			c++
		}

		// allocate exp+32 chars for the return string
		//mut res := []byte{len:exp+32,init:`0`}
		mut res := []byte{len: exp+32, init: 0}
		mut r_i := 0  // result string buffer index

		//println("s:${sgn} b:${b[0]} es:${exp_sgn} exp:${exp}")

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
			tmp_res := tos(res.data, dot_res_sp).clone()
			res.free()
			return tmp_res
		}
		
		//println("r_i-d_pos: ${r_i - d_pos}")
		if dot_res_sp >= 0 {
			if (r_i - dot_res_sp) > dec_digit {
				r_i = dot_res_sp + dec_digit + 1
			}
			res[r_i] = 0
			//println("result: [${tos(&res[0],r_i)}]")
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

// strings.Builder version of format_fl
[manualfree]
pub fn format_fl(f f64, p BF_param) string {
	unsafe{
		mut s  := ""
		//mut fs := "1.2343"
		mut fs := f64_to_str_lnd1(if f >= 0.0 {f} else {-f}, p.len1)
		//println("Dario")
		//println(fs)

		// error!!
		if fs[0] == `[` {
			s.free()
			return fs
		}

		if p.rm_tail_zero {
			tmp := fs
			fs = remove_tail_zeros(fs)
			tmp.free()
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
			tmp := s
			s = fs.clone()
			tmp.free()
		} else {
			if p.positive {
				if p.sign_flag {
					tmp := s
					s = "+" + fs
					tmp.free()
				} else {
					tmp := s
					s = fs.clone()
					tmp.free()
				}
			} else {
				tmp := s
				s = "-" + fs
				tmp.free()
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


		s.free()
		fs.free()
		tmp_res := res.str()
		res.free()
		return tmp_res
	}
}

[manualfree]
pub fn format_es(f f64, p BF_param) string {
	unsafe{
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
			tmp := s
			s = fs.clone()
			tmp.free()
		} else {
			if p.positive {
				if p.sign_flag {
					tmp := s
					s = "+" + fs
					tmp.free()
				} else {
					tmp := s
					s = fs.clone()
					tmp.free()
				}
			} else {
				tmp := s
				s = "-" + fs
				tmp.free()
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
		s.free()
		fs.free()
		tmp_res := res.str()
		res.free()
		return tmp_res
	}
}

[direct_array_access]
pub fn remove_tail_zeros(s string) string {
	unsafe{
		mut buf := malloc_noscan(s.len + 1)
		mut i_d := 0
		mut i_s := 0
		
		// skip spaces
		for i_s < s.len && s[i_s] !in [`-`,`+`] && (s[i_s] > `9` || s[i_s] < `0`) {
			buf[i_d] = s[i_s]
			i_s++
			i_d++
		}
		// sign
		if i_s < s.len && s[i_s] in [`-`,`+`] {
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
				sum += s[i_s1] - byte(`0`)
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
