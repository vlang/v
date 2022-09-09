/*=============================================================================
Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
=============================================================================*/
module strconv

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

// v_printf prints a sprintf-like formated `string` to the terminal.
[deprecated: 'use string interpolation instead']
pub fn v_printf(str string, pt ...voidptr) {
	print(v_sprintf(str, ...pt))
}

// v_sprintf returns a sprintf-like formated `string`.
//
// Example:
// ```v
// x := 3.141516
// assert strconv.v_sprintf('aaa %G', x) == 'aaa 3.141516'
// ```
[deprecated: 'use string interpolation instead']
[direct_array_access; manualfree]
pub fn v_sprintf(str string, pt ...voidptr) string {
	mut res := strings.new_builder(pt.len * 16)
	defer {
		unsafe { res.free() }
	}

	mut i := 0 // main string index
	mut p_index := 0 // parameter index
	mut sign := false // sign flag
	mut allign := Align_text.right
	mut len0 := -1 // forced length, if -1 free length
	mut len1 := -1 // decimal part for floats
	def_len1 := 6 // default value for len1
	mut pad_ch := u8(` `) // pad char

	// prefix chars for Length field
	mut ch1 := `0` // +1 char if present else `0`
	mut ch2 := `0` // +2 char if present else `0`

	mut status := Char_parse_state.norm_char
	for i < str.len {
		if status == .reset_params {
			sign = false
			allign = .right
			len0 = -1
			len1 = -1
			pad_ch = ` `
			status = .norm_char
			ch1 = `0`
			ch2 = `0`
			continue
		}

		ch := str[i]
		if ch != `%` && status == .norm_char {
			res.write_u8(ch)
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
			d1 := unsafe { *(&u8(pt[p_index])) }
			res.write_u8(d1)
			status = .reset_params
			p_index++
			i++
			continue
		}

		// pointer, manage it here
		if ch == `p` && status == .field_char {
			v_sprintf_panic(p_index, pt.len)
			res.write_string('0x')
			res.write_string(ptr_str(unsafe { pt[p_index] }))
			status = .reset_params
			p_index++
			i++
			continue
		}

		if status == .field_char {
			mut fc_ch1 := `0`
			mut fc_ch2 := `0`
			if (i + 1) < str.len {
				fc_ch1 = str[i + 1]
				if (i + 2) < str.len {
					fc_ch2 = str[i + 2]
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
			} else if ch in [`0`, ` `] {
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
				len := unsafe { *(&int(pt[p_index])) }
				p_index++
				v_sprintf_panic(p_index, pt.len)
				mut s := unsafe { *(&string(pt[p_index])) }
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
			} else if ch == `h` {
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
			else if ch in [`d`, `i`] {
				mut d1 := u64(0)
				mut positive := true

				// println("$ch1 $ch2")
				match ch1 {
					// h for 16 bit int
					// hh fot 8 bit int
					`h` {
						if ch2 == `h` {
							v_sprintf_panic(p_index, pt.len)
							x := unsafe { *(&i8(pt[p_index])) }
							positive = if x >= 0 { true } else { false }
							d1 = if positive { u64(x) } else { u64(-x) }
						} else {
							x := unsafe { *(&i16(pt[p_index])) }
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
						x := unsafe { *(&i64(pt[p_index])) }
						positive = if x >= 0 { true } else { false }
						d1 = if positive { u64(x) } else { u64(-x) }
					}
					// default int
					else {
						v_sprintf_panic(p_index, pt.len)
						x := unsafe { *(&int(pt[p_index])) }
						positive = if x >= 0 { true } else { false }
						d1 = if positive { u64(x) } else { u64(-x) }
					}
				}
				tmp := format_dec_old(d1,
					pad_ch: pad_ch
					len0: len0
					len1: 0
					positive: positive
					sign_flag: sign
					allign: allign
				)
				res.write_string(tmp)
				unsafe { tmp.free() }
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
							d1 = u64(unsafe { *(&u8(pt[p_index])) })
						} else {
							d1 = u64(unsafe { *(&u16(pt[p_index])) })
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
						d1 = u64(unsafe { *(&u64(pt[p_index])) })
					}
					// default int
					else {
						d1 = u64(unsafe { *(&u32(pt[p_index])) })
					}
				}

				tmp := format_dec_old(d1,
					pad_ch: pad_ch
					len0: len0
					len1: 0
					positive: positive
					sign_flag: sign
					allign: allign
				)
				res.write_string(tmp)
				unsafe { tmp.free() }
				status = .reset_params
				p_index++
				i++
				continue
			}
			// hex
			else if ch in [`x`, `X`] {
				v_sprintf_panic(p_index, pt.len)
				mut s := ''
				match ch1 {
					// h for 16 bit int
					// hh fot 8 bit int
					`h` {
						if ch2 == `h` {
							x := unsafe { *(&i8(pt[p_index])) }
							s = x.hex()
						} else {
							x := unsafe { *(&i16(pt[p_index])) }
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
						x := unsafe { *(&i64(pt[p_index])) }
						s = x.hex()
					}
					else {
						x := unsafe { *(&int(pt[p_index])) }
						s = x.hex()
					}
				}

				if ch == `X` {
					tmp := s
					s = s.to_upper()
					unsafe { tmp.free() }
				}

				tmp := format_str(s,
					pad_ch: pad_ch
					len0: len0
					len1: 0
					positive: true
					sign_flag: false
					allign: allign
				)
				res.write_string(tmp)
				unsafe { tmp.free() }
				unsafe { s.free() }
				status = .reset_params
				p_index++
				i++
				continue
			}

			// float and double
			if ch in [`f`, `F`] {
				$if !nofloat ? {
					v_sprintf_panic(p_index, pt.len)
					x := unsafe { *(&f64(pt[p_index])) }
					positive := x >= f64(0.0)
					len1 = if len1 >= 0 { len1 } else { def_len1 }
					s := format_fl_old(f64(x),
						pad_ch: pad_ch
						len0: len0
						len1: len1
						positive: positive
						sign_flag: sign
						allign: allign
					)
					if ch == `F` {
						tmp := s.to_upper()
						res.write_string(tmp)
						unsafe { tmp.free() }
					} else {
						res.write_string(s)
					}
					unsafe { s.free() }
				}
				status = .reset_params
				p_index++
				i++
				continue
			} else if ch in [`e`, `E`] {
				$if !nofloat ? {
					v_sprintf_panic(p_index, pt.len)
					x := unsafe { *(&f64(pt[p_index])) }
					positive := x >= f64(0.0)
					len1 = if len1 >= 0 { len1 } else { def_len1 }
					s := format_es_old(f64(x),
						pad_ch: pad_ch
						len0: len0
						len1: len1
						positive: positive
						sign_flag: sign
						allign: allign
					)
					if ch == `E` {
						tmp := s.to_upper()
						res.write_string(tmp)
						unsafe { tmp.free() }
					} else {
						res.write_string(s)
					}
					unsafe { s.free() }
				}
				status = .reset_params
				p_index++
				i++
				continue
			} else if ch in [`g`, `G`] {
				$if !nofloat ? {
					v_sprintf_panic(p_index, pt.len)
					x := unsafe { *(&f64(pt[p_index])) }
					positive := x >= f64(0.0)
					mut s := ''
					tx := fabs(x)
					if tx < 999_999.0 && tx >= 0.00001 {
						// println("Here g format_fl [$tx]")
						len1 = if len1 >= 0 { len1 + 1 } else { def_len1 }
						tmp := s
						s = format_fl_old(x,
							pad_ch: pad_ch
							len0: len0
							len1: len1
							positive: positive
							sign_flag: sign
							allign: allign
							rm_tail_zero: true
						)
						unsafe { tmp.free() }
					} else {
						len1 = if len1 >= 0 { len1 + 1 } else { def_len1 }
						tmp := s
						s = format_es_old(x,
							pad_ch: pad_ch
							len0: len0
							len1: len1
							positive: positive
							sign_flag: sign
							allign: allign
							rm_tail_zero: true
						)
						unsafe { tmp.free() }
					}
					if ch == `G` {
						tmp := s.to_upper()
						res.write_string(tmp)
						unsafe { tmp.free() }
					} else {
						res.write_string(s)
					}
					unsafe { s.free() }
				}
				status = .reset_params
				p_index++
				i++
				continue
			}
			// string
			else if ch == `s` {
				v_sprintf_panic(p_index, pt.len)
				s1 := unsafe { *(&string(pt[p_index])) }
				pad_ch = ` `
				tmp := format_str(s1,
					pad_ch: pad_ch
					len0: len0
					len1: 0
					positive: true
					sign_flag: false
					allign: allign
				)
				res.write_string(tmp)
				unsafe { tmp.free() }
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
		panic('$p_index % conversion specifiers, but given $pt.len args')
	}

	return res.str()
}

[inline]
fn v_sprintf_panic(idx int, len int) {
	if idx >= len {
		panic('${idx + 1} % conversion specifiers, but given only $len args')
	}
}

fn fabs(x f64) f64 {
	if x < 0.0 {
		return -x
	}
	return x
}

// strings.Builder version of format_fl
[direct_array_access; manualfree]
pub fn format_fl_old(f f64, p BF_param) string {
	unsafe {
		mut s := ''
		// mut fs := "1.2343"
		mut fs := f64_to_str_lnd1(if f >= 0.0 { f } else { -f }, p.len1)
		// println("Dario")
		// println(fs)

		// error!!
		if fs[0] == `[` {
			s.free()
			return fs
		}

		if p.rm_tail_zero {
			tmp := fs
			fs = remove_tail_zeros_old(fs)
			tmp.free()
		}
		mut res := strings.new_builder(if p.len0 > fs.len { p.len0 } else { fs.len })
		defer {
			res.free()
		}

		mut sign_len_diff := 0
		if p.pad_ch == `0` {
			if p.positive {
				if p.sign_flag {
					res.write_u8(`+`)
					sign_len_diff = -1
				}
			} else {
				res.write_u8(`-`)
				sign_len_diff = -1
			}
			tmp := s
			s = fs.clone()
			tmp.free()
		} else {
			if p.positive {
				if p.sign_flag {
					tmp := s
					s = '+' + fs
					tmp.free()
				} else {
					tmp := s
					s = fs.clone()
					tmp.free()
				}
			} else {
				tmp := s
				s = '-' + fs
				tmp.free()
			}
		}

		dif := p.len0 - s.len + sign_len_diff

		if p.allign == .right {
			for i1 := 0; i1 < dif; i1++ {
				res.write_u8(p.pad_ch)
			}
		}
		res.write_string(s)
		if p.allign == .left {
			for i1 := 0; i1 < dif; i1++ {
				res.write_u8(p.pad_ch)
			}
		}

		s.free()
		fs.free()
		return res.str()
	}
}

[manualfree]
fn format_es_old(f f64, p BF_param) string {
	unsafe {
		mut s := ''
		mut fs := f64_to_str_pad(if f > 0 { f } else { -f }, p.len1)
		if p.rm_tail_zero {
			tmp := fs
			fs = remove_tail_zeros_old(fs)
			tmp.free()
		}
		mut res := strings.new_builder(if p.len0 > fs.len { p.len0 } else { fs.len })
		defer {
			res.free()
			fs.free()
			s.free()
		}

		mut sign_len_diff := 0
		if p.pad_ch == `0` {
			if p.positive {
				if p.sign_flag {
					res.write_u8(`+`)
					sign_len_diff = -1
				}
			} else {
				res.write_u8(`-`)
				sign_len_diff = -1
			}
			tmp := s
			s = fs.clone()
			tmp.free()
		} else {
			if p.positive {
				if p.sign_flag {
					tmp := s
					s = '+' + fs
					tmp.free()
				} else {
					tmp := s
					s = fs.clone()
					tmp.free()
				}
			} else {
				tmp := s
				s = '-' + fs
				tmp.free()
			}
		}

		dif := p.len0 - s.len + sign_len_diff
		if p.allign == .right {
			for i1 := 0; i1 < dif; i1++ {
				res.write_u8(p.pad_ch)
			}
		}
		res.write_string(s)
		if p.allign == .left {
			for i1 := 0; i1 < dif; i1++ {
				res.write_u8(p.pad_ch)
			}
		}
		return res.str()
	}
}

fn remove_tail_zeros_old(s string) string {
	mut i := 0
	mut last_zero_start := -1
	mut dot_pos := -1
	mut in_decimal := false
	mut prev_ch := u8(0)
	for i < s.len {
		ch := unsafe { s.str[i] }
		if ch == `.` {
			in_decimal = true
			dot_pos = i
		} else if in_decimal {
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

	mut tmp := ''
	if last_zero_start > 0 {
		if last_zero_start == dot_pos + 1 {
			tmp = s[..dot_pos] + s[i..]
		} else {
			tmp = s[..last_zero_start] + s[i..]
		}
	} else {
		tmp = s.clone()
	}
	if unsafe { tmp.str[tmp.len - 1] } == `.` {
		return tmp[..tmp.len - 1]
	}
	return tmp
}

// max int64 9223372036854775807
[manualfree]
pub fn format_dec_old(d u64, p BF_param) string {
	mut s := ''
	mut res := strings.new_builder(20)
	defer {
		unsafe { res.free() }
		unsafe { s.free() }
	}
	mut sign_len_diff := 0
	if p.pad_ch == `0` {
		if p.positive {
			if p.sign_flag {
				res.write_u8(`+`)
				sign_len_diff = -1
			}
		} else {
			res.write_u8(`-`)
			sign_len_diff = -1
		}
		tmp := s
		s = d.str()
		unsafe { tmp.free() }
	} else {
		if p.positive {
			if p.sign_flag {
				tmp := s
				s = '+' + d.str()
				unsafe { tmp.free() }
			} else {
				tmp := s
				s = d.str()
				unsafe { tmp.free() }
			}
		} else {
			tmp := s
			s = '-' + d.str()
			unsafe { tmp.free() }
		}
	}
	dif := p.len0 - s.len + sign_len_diff

	if p.allign == .right {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	res.write_string(s)
	if p.allign == .left {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	return res.str()
}
