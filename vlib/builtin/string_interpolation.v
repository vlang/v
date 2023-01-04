module builtin

import strconv
import strings

/*=============================================================================
Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
=============================================================================*/

/*============================================================================
Enum format types max 0x1F => 32 types
=============================================================================*/
pub enum StrIntpType {
	si_no_str = 0 // no parameter to print only fix string
	si_c
	si_u8
	si_i8
	si_u16
	si_i16
	si_u32
	si_i32
	si_u64
	si_i64
	si_e32
	si_e64
	si_f32
	si_f64
	si_g32
	si_g64
	si_s
	si_p
	si_vp
}

pub fn (x StrIntpType) str() string {
	return match x {
		.si_no_str { 'no_str' }
		.si_c { 'c' }
		.si_u8 { 'u8' }
		.si_i8 { 'i8' }
		.si_u16 { 'u16' }
		.si_i16 { 'i16' }
		.si_u32 { 'u32' }
		.si_i32 { 'i32' }
		.si_u64 { 'u64' }
		.si_i64 { 'i64' }
		.si_f32 { 'f32' }
		.si_f64 { 'f64' }
		.si_g32 { 'f32' } // g32 format use f32 data
		.si_g64 { 'f64' } // g64 format use f64 data
		.si_e32 { 'f32' } // e32 format use f32 data
		.si_e64 { 'f64' } // e64 format use f64 data
		.si_s { 's' }
		.si_p { 'p' }
		.si_vp { 'vp' }
	}
}

// Union data used by StrIntpData
pub union StrIntpMem {
pub mut:
	d_c   u32
	d_u8  byte
	d_i8  i8
	d_u16 u16
	d_i16 i16
	d_u32 u32
	d_i32 int
	d_u64 u64
	d_i64 i64
	d_f32 f32
	d_f64 f64
	d_s   string
	d_p   voidptr
	d_vp  voidptr
}

[inline]
fn fabs32(x f32) f32 {
	return if x < 0 { -x } else { x }
}

[inline]
fn fabs64(x f64) f64 {
	return if x < 0 { -x } else { x }
}

[inline]
fn abs64(x i64) u64 {
	return if x < 0 { u64(-x) } else { u64(x) }
}

//  u32/u64 bit compact format
//___     32      24      16       8
//___      |       |       |       |
//_3333333333222222222211111111110000000000
//_9876543210987654321098765432109876543210
//_nPPPPPPPPBBBBWWWWWWWWWWTDDDDDDDSUAA=====
// = data type  5 bit  max 32 data type
// A allign     2 bit  Note: for now only 1 used!
// U uppercase  1 bit  0 do nothing, 1 do to_upper()
// S sign       1 bit  show the sign if positive
// D decimals   7 bit  number of decimals digit to show
// T tail zeros 1 bit  1 remove tail zeros, 0 do nothing
// W Width     10 bit  number of char for padding and indentation
// B num base   4 bit  start from 2, 0 for base 10
// P pad char 1/8 bit  padding char (in u32 format reduced to 1 bit as flag for `0` padding)
//     --------------
//     TOTAL:  39/32 bit
//---------------------------------------

// convert from data format to compact u64
pub fn get_str_intp_u64_format(fmt_type StrIntpType, in_width int, in_precision int, in_tail_zeros bool, in_sign bool, in_pad_ch byte, in_base int, in_upper_case bool) u64 {
	width := if in_width != 0 { abs64(in_width) } else { u64(0) }
	allign := if in_width > 0 { u64(1 << 5) } else { u64(0) } // two bit 0 .left 1 .rigth, for now we use only one
	upper_case := if in_upper_case { u64(1 << 7) } else { u64(0) }
	sign := if in_sign { u64(1 << 8) } else { u64(0) }
	precision := if in_precision != 987698 {
		(u64(in_precision & 0x7F) << 9)
	} else {
		u64(0x7F) << 9
	}
	tail_zeros := if in_tail_zeros { u32(1) << 16 } else { u32(0) }
	base := u64(u32(in_base & 0xf) << 27)
	res := u64((u64(fmt_type) & 0x1F) | allign | upper_case | sign | precision | tail_zeros | (u64(width & 0x3FF) << 17) | base | (u64(in_pad_ch) << 31))
	return res
}

// convert from data format to compact u32
pub fn get_str_intp_u32_format(fmt_type StrIntpType, in_width int, in_precision int, in_tail_zeros bool, in_sign bool, in_pad_ch byte, in_base int, in_upper_case bool) u32 {
	width := if in_width != 0 { abs64(in_width) } else { u32(0) }
	allign := if in_width > 0 { u32(1 << 5) } else { u32(0) } // two bit 0 .left 1 .rigth, for now we use only one
	upper_case := if in_upper_case { u32(1 << 7) } else { u32(0) }
	sign := if in_sign { u32(1 << 8) } else { u32(0) }
	precision := if in_precision != 987698 {
		(u32(in_precision & 0x7F) << 9)
	} else {
		u32(0x7F) << 9
	}
	tail_zeros := if in_tail_zeros { u32(1) << 16 } else { u32(0) }
	base := u32(u32(in_base & 0xf) << 27)
	res := u32((u32(fmt_type) & 0x1F) | allign | upper_case | sign | precision | tail_zeros | (u32(width & 0x3FF) << 17) | base | (u32(in_pad_ch & 1) << 31))
	return res
}

// convert from struct to formated string
[manualfree]
fn (data &StrIntpData) process_str_intp_data(mut sb strings.Builder) {
	x := data.fmt
	typ := unsafe { StrIntpType(x & 0x1F) }
	allign := int((x >> 5) & 0x01)
	upper_case := ((x >> 7) & 0x01) > 0
	sign := int((x >> 8) & 0x01)
	precision := int((x >> 9) & 0x7F)
	tail_zeros := ((x >> 16) & 0x01) > 0
	width := int(i16((x >> 17) & 0x3FF))
	mut base := int(x >> 27) & 0xF
	fmt_pad_ch := u8((x >> 31) & 0xFF)

	// no string interpolation is needed, return empty string
	if typ == .si_no_str {
		return
	}

	// if width > 0 { println("${x.hex()} Type: ${x & 0x7F} Width: ${width} Precision: ${precision} allign:${allign}") }

	// manage base if any
	if base > 0 {
		base += 2 // we start from 2, 0 == base 10
	}

	// mange pad char, for now only 0 allowed
	mut pad_ch := u8(` `)
	if fmt_pad_ch > 0 {
		// pad_ch = fmt_pad_ch
		pad_ch = `0`
	}

	len0_set := if width > 0 { width } else { -1 }
	len1_set := if precision == 0x7F { -1 } else { precision }
	sign_set := sign == 1

	mut bf := strconv.BF_param{
		pad_ch: pad_ch // padding char
		len0: len0_set // default len for whole the number or string
		len1: len1_set // number of decimal digits, if needed
		positive: true // mandatory: the sign of the number passed
		sign_flag: sign_set // flag for print sign as prefix in padding
		allign: .left // alignment of the string
		rm_tail_zero: tail_zeros // false // remove the tail zeros from floats
	}

	// allign
	if fmt_pad_ch == 0 {
		match allign {
			0 { bf.allign = .left }
			1 { bf.allign = .right }
			// 2 { bf.allign = .center }
			else { bf.allign = .left }
		}
	} else {
		bf.allign = .right
	}

	unsafe {
		// strings
		if typ == .si_s {
			mut s := ''
			if upper_case {
				s = data.d.d_s.to_upper()
			} else {
				s = data.d.d_s.clone()
			}
			if width == 0 {
				sb.write_string(s)
			} else {
				strconv.format_str_sb(s, bf, mut sb)
			}
			s.free()
			return
		}

		// signed int
		if typ in [.si_i8, .si_i16, .si_i32, .si_i64] {
			mut d := data.d.d_i64
			if typ == .si_i8 {
				d = i64(data.d.d_i8)
			} else if typ == .si_i16 {
				d = i64(data.d.d_i16)
			} else if typ == .si_i32 {
				d = i64(data.d.d_i32)
			}

			if base == 0 {
				if width == 0 {
					d_str := d.str()
					sb.write_string(d_str)
					d_str.free()
					return
				}
				if d < 0 {
					bf.positive = false
				}
				strconv.format_dec_sb(abs64(d), bf, mut sb)
			} else {
				// binary, we use 3 for binary
				if base == 3 {
					base = 2
				}
				mut absd, mut write_minus := d, false
				if d < 0 && pad_ch != ` ` {
					absd = -d
					write_minus = true
				}
				mut hx := strconv.format_int(absd, base)
				if upper_case {
					tmp := hx
					hx = hx.to_upper()
					tmp.free()
				}
				if write_minus {
					sb.write_u8(`-`)
					bf.len0-- // compensate for the `-` above
				}
				if width == 0 {
					sb.write_string(hx)
				} else {
					strconv.format_str_sb(hx, bf, mut sb)
				}
				hx.free()
			}
			return
		}

		// unsigned int and pointers
		if typ in [.si_u8, .si_u16, .si_u32, .si_u64] {
			mut d := data.d.d_u64
			if typ == .si_u8 {
				d = u64(data.d.d_u8)
			} else if typ == .si_u16 {
				d = u64(data.d.d_u16)
			} else if typ == .si_u32 {
				d = u64(data.d.d_u32)
			}
			if base == 0 {
				if width == 0 {
					d_str := d.str()
					sb.write_string(d_str)
					d_str.free()
					return
				}
				strconv.format_dec_sb(d, bf, mut sb)
			} else {
				// binary, we use 3 for binary
				if base == 3 {
					base = 2
				}
				mut hx := strconv.format_uint(d, base)
				if upper_case {
					tmp := hx
					hx = hx.to_upper()
					tmp.free()
				}
				if width == 0 {
					sb.write_string(hx)
				} else {
					strconv.format_str_sb(hx, bf, mut sb)
				}
				hx.free()
			}
			return
		}

		// pointers
		if typ == .si_p {
			mut d := data.d.d_u64
			base = 16 // TODO: **** decide the behaviour of this flag! ****
			if base == 0 {
				if width == 0 {
					d_str := d.str()
					sb.write_string(d_str)
					d_str.free()
					return
				}
				strconv.format_dec_sb(d, bf, mut sb)
			} else {
				mut hx := strconv.format_uint(d, base)
				if upper_case {
					tmp := hx
					hx = hx.to_upper()
					tmp.free()
				}
				if width == 0 {
					sb.write_string(hx)
				} else {
					strconv.format_str_sb(hx, bf, mut sb)
				}
				hx.free()
			}
			return
		}

		// default settings for floats
		mut use_default_str := false
		if width == 0 && precision == 0x7F {
			bf.len1 = 3
			use_default_str = true
		}
		if bf.len1 < 0 {
			bf.len1 = 3
		}

		match typ {
			// floating point
			.si_f32 {
				$if !nofloat ? {
					// println("HERE: f32")
					if use_default_str {
						mut f := data.d.d_f32.str()
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					} else {
						// println("HERE: f32 format")
						// println(data.d.d_f32)
						if data.d.d_f32 < 0 {
							bf.positive = false
						}
						mut f := strconv.format_fl(data.d.d_f32, bf)
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					}
				}
			}
			.si_f64 {
				$if !nofloat ? {
					// println("HERE: f64")
					if use_default_str {
						mut f := data.d.d_f64.str()
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					} else {
						if data.d.d_f64 < 0 {
							bf.positive = false
						}
						f_union := strconv.Float64u{
							f: data.d.d_f64
						}
						if f_union.u == strconv.double_minus_zero {
							bf.positive = false
						}

						mut f := strconv.format_fl(data.d.d_f64, bf)
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					}
				}
			}
			.si_g32 {
				// println("HERE: g32")
				if use_default_str {
					$if !nofloat ? {
						mut f := data.d.d_f32.strg()
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					}
				} else {
					// Manage +/-0
					if data.d.d_f32 == strconv.single_plus_zero {
						tmp_str := '0'
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
						return
					}
					if data.d.d_f32 == strconv.single_minus_zero {
						tmp_str := '-0'
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
						return
					}
					// Manage +/-INF
					if data.d.d_f32 == strconv.single_plus_infinity {
						mut tmp_str := '+inf'
						if upper_case {
							tmp_str = '+INF'
						}
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
					}
					if data.d.d_f32 == strconv.single_minus_infinity {
						mut tmp_str := '-inf'
						if upper_case {
							tmp_str = '-INF'
						}
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
					}

					if data.d.d_f32 < 0 {
						bf.positive = false
					}
					d := fabs32(data.d.d_f32)
					if d < 999_999.0 && d >= 0.00001 {
						mut f := strconv.format_fl(data.d.d_f32, bf)
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
						return
					}
					mut f := strconv.format_es(data.d.d_f32, bf)
					if upper_case {
						tmp := f
						f = f.to_upper()
						tmp.free()
					}
					sb.write_string(f)
					f.free()
				}
			}
			.si_g64 {
				// println("HERE: g64")
				if use_default_str {
					$if !nofloat ? {
						mut f := data.d.d_f64.strg()
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					}
				} else {
					// Manage +/-0
					if data.d.d_f64 == strconv.double_plus_zero {
						tmp_str := '0'
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
						return
					}
					if data.d.d_f64 == strconv.double_minus_zero {
						tmp_str := '-0'
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
						return
					}
					// Manage +/-INF
					if data.d.d_f64 == strconv.double_plus_infinity {
						mut tmp_str := '+inf'
						if upper_case {
							tmp_str = '+INF'
						}
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
					}
					if data.d.d_f64 == strconv.double_minus_infinity {
						mut tmp_str := '-inf'
						if upper_case {
							tmp_str = '-INF'
						}
						strconv.format_str_sb(tmp_str, bf, mut sb)
						tmp_str.free()
					}

					if data.d.d_f64 < 0 {
						bf.positive = false
					}
					d := fabs64(data.d.d_f64)
					if d < 999_999.0 && d >= 0.00001 {
						mut f := strconv.format_fl(data.d.d_f64, bf)
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
						return
					}
					mut f := strconv.format_es(data.d.d_f64, bf)
					if upper_case {
						tmp := f
						f = f.to_upper()
						tmp.free()
					}
					sb.write_string(f)
					f.free()
				}
			}
			.si_e32 {
				$if !nofloat ? {
					// println("HERE: e32")
					bf.len1 = 6
					if use_default_str {
						mut f := data.d.d_f32.str()
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					} else {
						if data.d.d_f32 < 0 {
							bf.positive = false
						}
						mut f := strconv.format_es(data.d.d_f32, bf)
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					}
				}
			}
			.si_e64 {
				$if !nofloat ? {
					// println("HERE: e64")
					bf.len1 = 6
					if use_default_str {
						mut f := data.d.d_f64.str()
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					} else {
						if data.d.d_f64 < 0 {
							bf.positive = false
						}
						mut f := strconv.format_es(data.d.d_f64, bf)
						if upper_case {
							tmp := f
							f = f.to_upper()
							tmp.free()
						}
						sb.write_string(f)
						f.free()
					}
				}
			}
			// runes
			.si_c {
				ss := utf32_to_str(data.d.d_c)
				sb.write_string(ss)
				ss.free()
			}
			// v pointers
			.si_vp {
				ss := u64(data.d.d_vp).hex()
				sb.write_string(ss)
				ss.free()
			}
			else {
				sb.write_string('***ERROR!***')
			}
		}
	}
}

//--------------------------------------------------

// storing struct used by cgen
pub struct StrIntpCgenData {
pub:
	str string
	fmt string
	d   string
}

// NOTE: LOW LEVEL struct
// storing struct passed to V in the C code
pub struct StrIntpData {
pub:
	str string
	// fmt     u64  // expanded version for future use, 64 bit
	fmt u32
	d   StrIntpMem
}

// interpolation function
[direct_array_access; manualfree]
pub fn str_intp(data_len int, in_data voidptr) string {
	mut res := strings.new_builder(256)
	input_base := &StrIntpData(in_data)
	for i := 0; i < data_len; i++ {
		data := unsafe { &input_base[i] }
		// avoid empty strings
		if data.str.len != 0 {
			res.write_string(data.str)
		}
		// skip empty data
		if data.fmt != 0 {
			data.process_str_intp_data(mut res)
		}
	}
	ret := res.str()
	unsafe { res.free() }
	return ret
}

// The consts here are utilities for the compiler's "auto_str_methods.v".
// They are used to substitute old _STR calls.
// FIXME: this const is not released from memory => use a precalculated string const for now.
// si_s_code = "0x" + int(StrIntpType.si_s).hex() // code for a simple string.
pub const (
	si_s_code   = '0xfe10'
	si_g32_code = '0xfe0e'
	si_g64_code = '0xfe0f'
)

[inline]
pub fn str_intp_sq(in_str string) string {
	return 'str_intp(2, _MOV((StrIntpData[]){{_SLIT("\'"), ${si_s_code}, {.d_s = ${in_str}}},{_SLIT("\'"), 0, {.d_c = 0 }}}))'
}

[inline]
pub fn str_intp_rune(in_str string) string {
	return 'str_intp(2, _MOV((StrIntpData[]){{_SLIT("\`"), ${si_s_code}, {.d_s = ${in_str}}},{_SLIT("\`"), 0, {.d_c = 0 }}}))'
}

[inline]
pub fn str_intp_g32(in_str string) string {
	return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${si_g32_code}, {.d_f32 = ${in_str} }}}))'
}

[inline]
pub fn str_intp_g64(in_str string) string {
	return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${si_g64_code}, {.d_f64 = ${in_str} }}}))'
}

// replace %% with the in_str
[manualfree]
pub fn str_intp_sub(base_str string, in_str string) string {
	index := base_str.index('%%') or {
		eprintln('No strin interpolation %% parameteres')
		exit(1)
	}
	// return base_str[..index] + in_str + base_str[index+2..]

	unsafe {
		st_str := base_str[..index]
		if index + 2 < base_str.len {
			en_str := base_str[index + 2..]
			res_str := 'str_intp(2, _MOV((StrIntpData[]){{_SLIT("${st_str}"), ${si_s_code}, {.d_s = ${in_str} }},{_SLIT("${en_str}"), 0, {.d_c = 0}}}))'
			st_str.free()
			en_str.free()
			return res_str
		}
		res2_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("${st_str}"), ${si_s_code}, {.d_s = ${in_str} }}}))'
		st_str.free()
		return res2_str
	}
}
