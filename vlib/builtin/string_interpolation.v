/*=============================================================================
Copyright (c) 2019-2021 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
=============================================================================*/

module builtin
import strconv
import strings

//=============================================================================
// Enum format types max 0x1F => 32 types
//=============================================================================
pub enum StrIntpType {
	si_no_str = 0  // no parameter to print only fix string
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
	match x {
		.si_no_str{ return "no_str" }
		.si_c     { return "c" }
		
		.si_u8    { return "u8" }
		.si_i8    { return "i8" }
		.si_u16   { return "u16" }
		.si_i16   { return "i16" }
		.si_u32   { return "u32" }
		.si_i32   { return "i32" }
		.si_u64   { return "u64" }
		.si_i64   { return "i64" }
		
		.si_f32   { return "f32" }
		.si_f64   { return "f64" }
		.si_g32   { return "f32" }  // g32 format use f32 data
		.si_g64   { return "f64" }  // g64 format use f64 data
		.si_e32   { return "f32" }  // e32 format use f32 data
		.si_e64   { return "f64" }  // e64 format use f64 data

		.si_s     { return "s" }
		.si_p     { return "p" }
		.si_vp    { return "vp" }
	}
}

//=============================================================================
// Union data
//=============================================================================
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

fn abs(x int) int {
	if x < 0 { return -x }
	return x
}

fn fabs64(x f64) f64 {
	if x < 0 { return -x }
	return x
}

fn fabs32(x f32) f32 {
	if x < 0 { return -x }
	return x
}

fn abs64(x i64) u64 {
	if x < 0 { return u64(-x) }
	return u64(x)
}

//=========================================
//
//  u64 bit compact format
//
//      32      24      16       8     
//       |       |       |       | 
//3333333333222222222211111111110000000000
//9876543210987654321098765432109876543210
// PPPPPPPPBBBBWWWWWWWWWWDDDDDDDDSUAA=====
// = data type  5 bit  max 32 data type
// A allign     2 bit  Note: for now only 1 used!
// U uppercase  1 bit  0 do nothing, 1 do to_upper()
// S sign       1 bit  show the sign if positive
// D decimals   8 bit  number of decimals digit to show
// W Width     10 bit  number of char for padding and indentation
// B num base   4 bit  start from 2, 0 for base 10
// P pad char   8 bit  padding char
//     --------------
//     TOTAL:  39 bit
//=========================================

// convert from data format to compact u64
pub fn get_str_intp_u64_format(fmt_type StrIntpType, in_width int, in_precision int, in_sign bool, in_pad_ch u8, in_base int, in_upper_case bool) u64 {
	width      := if in_width != 0 { abs64(in_width) } else { u64(0) }
	allign     := if in_width > 0 { u64(1 << 5) } else { u64(0) }  // two bit 0 .left 1 .rigth, for now we use only one
	upper_case := if in_upper_case { u64(1 << 7) } else { u64(0) }
	sign       := if in_sign { u64(1 << 8) } else { u64(0) }
	precision  := if in_precision != 987698 { (u64(in_precision & 0xFF) << 9)  } else { u64(0xFF) << 9 }
	base       := u64((in_base & 0xf) << 27)
	res := u64( (u64(fmt_type) & 0x1F) | allign | upper_case | sign |  precision | (u64(width & 0x3FF) << 17) | base | (u64(in_pad_ch) << 31) )
	return res
}

// convert from struct to formated string
[manualfree]
fn (data StrIntpData) get_fmt_from_u64_format1(mut sb &strings.Builder) {
	x              := data.fmt
	typ            := StrIntpType(x & 0x1F)
	allign         := int((x >> 5) & 0x01)
	upper_case     := if ((x >> 7) & 0x01) > 0 { true } else { false }
	sign           := int((x >> 8) & 0x01)
	precision      := int((x >> 9) & 0xFF)
	width          := int(i16((x >> 17) & 0x3FF))
	mut base       := int(x >> 27) & 0xF
	fmt_pad_ch     := byte((x >> 31) & 0xFF)
	
	// no string interpolation is needed, return empty string
	if typ == .si_no_str { return }

	//if width > 0 { println("${x.hex()} Type: ${x & 0x7F} Width: ${width} Precision: ${precision} allign:${allign}") }

	// manage base if any
	if base > 0 {
		base += 2 // we start from 2, 0 == base 10
	}	
	
	// mange pad char, for now only 0 allowed
	mut pad_ch := byte(` `)
	if fmt_pad_ch > 0 {
		pad_ch = fmt_pad_ch
	}

	len0_set := if width > 0 { width } else { -1 }
	len1_set := if precision == 0xFF { -1 } else {precision}
	sign_set := if sign == 1 {true} else {false} 


	mut bf := strconv.BF_param {
		pad_ch       : pad_ch    // padding char
		len0         : len0_set  // default len for whole the number or string
		len1         : len1_set  // number of decimal digits, if needed
		positive     : true      // mandatory: the sign of the number passed
		sign_flag    : sign_set  // flag for print sign as prefix in padding
		allign       : .left     // alignment of the string
		rm_tail_zero : false     // remove the tail zeros from floats
	}

	// allign
	if fmt_pad_ch == 0 {
		match allign {
			0 { bf.allign = .left }
			1 { bf.allign = .right }
			// 2 { bf.allign = .center }
			else {bf.allign = .left }
		}
	} else {
		bf.allign = .right
	}

	unsafe {
		// strings
		if typ == .si_s   { 
			mut d := ''
			if upper_case {
				d = data.d.d_s.to_upper()
			} else {
				d = data.d.d_s.clone()
			}
			if width == 0 {
				sb.write_string(d)
				d.free()
			} else {
				n := strconv.format_str(d, bf)
				d.free()
				sb.write_string(n)
				n.free()
			}
			return
		}
	
		// signed int
		if typ in [.si_i8, .si_i16, .si_i32, .si_i64] {
			mut d := data.d.d_i64
			if typ == .si_i8 { d = i64(data.d.d_i8) }
			else if typ == .si_i16 { d = i64(data.d.d_i16) }
			else if typ == .si_i32 { d = i64(data.d.d_i32) }

			if base == 0 {
				if width == 0 {
					sb.write_string(d.str())
					return
				}
				if d < 0 { bf.positive = false }
				sb.write_string(strconv.format_dec(abs64(d), bf))
			} else {
				mut hx := strconv.format_uint(data.d.d_u64, base)
				if upper_case {
					o := hx
					hx = hx.to_upper()
					o.free()
				}
				if width == 0 {
					sb.write_string(hx)
				} else {
					hx_formatted := strconv.format_str(hx, bf)
					sb.write_string(hx_formatted)
					hx_formatted.free()
				}
				hx.free()
			}
			return
		}

		// unsigned int and pointers
		if typ in [.si_u8, .si_u16, .si_u32, .si_u64] {
			mut d := data.d.d_u64
			if base == 0 {
				if width == 0 {
					sb.write_string(d.str())
					return
				}
				sb.write_string(strconv.format_dec(d, bf))
			} else {
				mut hx := strconv.format_uint(d, base)
				if upper_case { 
					o := hx
					hx = hx.to_upper()
					o.free()
				}
				if width == 0 {
					sb.write_string(hx)
				} else {
					hx_formatted := strconv.format_str(hx, bf)
					sb.write_string( hx_formatted )
					hx_formatted.free()
				}
				hx.free()
			}
			return
		}

		// pointers
		if typ in [.si_p] {
			mut d := data.d.d_u64
			base = 16  // TODO: **** decide the behaviour of this flag! ****
			if base == 0 {
				if width == 0 {
					dstr := d.str()
					sb.write_string(dstr)
					dstr.free()
					return
				}
				d_formatted := strconv.format_dec(d, bf)
				sb.write_string(d_formatted)
				d_formatted.free()
			} else {
				mut hx := strconv.format_uint(d, base)
				if upper_case { 
					o := hx
					hx = hx.to_upper()
					o.free()
				}
				if width == 0 {
					sb.write_string(hx)
				} else {
					hx_formatted := strconv.format_str(hx, bf)
					sb.write_string( hx_formatted )
					hx_formatted.free()
				}
				hx.free()
			}
			return
		} 

		// default settings for floats
		mut use_default_str := false
		if width == 0 && precision == 0xFF {
			bf.len1 = 3
			use_default_str = true
		}

		match typ {	
			// floating point
			.si_f32 { 
				if use_default_str {
					sb.write_string(data.d.d_f32.str())
				} else {
					if data.d.d_f32 < 0 { bf.positive = false }
					sb.write_string(strconv.format_fl(data.d.d_f32, bf))
				}
			}
			.si_f64 { 
				if use_default_str {
					sb.write_string(data.d.d_f64.str())
				} else {
					if data.d.d_f64 < 0 { bf.positive = false }
					sb.write_string(strconv.format_fl(data.d.d_f64, bf))
				}
			}
			.si_g32 {
				if use_default_str {
					sb.write_string(data.d.d_f32.str())
				} else {
					if data.d.d_f32 < 0 { bf.positive = false }
					d := fabs32(data.d.d_f32)
					if d < 999_999.0 && d >= 0.00001 {
						sb.write_string(strconv.format_fl(data.d.d_f32, bf))
						return
					}
					sb.write_string(strconv.format_es(data.d.d_f32, bf))
				}
			}
			.si_g64 { 
				if use_default_str {
					sb.write_string(data.d.d_f64.str())
				} else {
					if data.d.d_f64 < 0 { bf.positive = false }
					d := fabs64(data.d.d_f64)
					if d < 999_999.0 && d >= 0.00001 {
						sb.write_string(strconv.format_fl(data.d.d_f64, bf))
						return
					}
					sb.write_string(strconv.format_es(data.d.d_f64, bf))
				}
			}
			.si_e32 { 
				if use_default_str {
					sb.write_string(data.d.d_f32.str())
				} else {
					if data.d.d_f32 < 0 { bf.positive = false }
					sb.write_string(strconv.format_es(data.d.d_f32, bf))
				}
			}
			.si_e64 { 
				if use_default_str {
					sb.write_string(data.d.d_f64.str())
				} else {
					if data.d.d_f64 < 0 { bf.positive = false }
					sb.write_string(strconv.format_es(data.d.d_f64, bf))
				}
			}

			// runes
			.si_c   { sb.write_string(utf32_to_str(data.d.d_c)) }

			// v pointers
			.si_vp  {
				sb.write_string(u64(data.d.d_vp).hex())
			}

			else { sb.write_string("***ERROR!***")}

		}
		
	}

}

//====================================================================================

// storing struct used by cgen
pub struct StrIntpCgenData {
pub:
	str     string
	fmt     string
	d       string
}

// NOTE: LOW LEVEL struct 
// storing struct passed to V in the C code
pub struct StrIntpData {
pub:
	str     string
	fmt     u64
	d       StrIntpMem
}

// interpolation function
[manualfree]
pub fn str_interpolation(data_len int, in_data voidptr) string {
	mut res := strings.new_builder(256)
	defer {
		unsafe{res.free()}
	}
	//mut res := ""
	unsafe{	
		mut i := 0
		for i < data_len {
			data := &StrIntpData( byteptr(in_data) + (int(sizeof(StrIntpData)) * i) )
			res.write_string(data.str)
			//res += data.str
			// skip only string records
			if data.fmt != 0 {
				data.get_fmt_from_u64_format(mut &res)
				//res += data.get_fmt_from_u64_format()
				//res.write_string(data.get_fmt_from_u64_format())
			}
			i++
		}
	}
	//return res
	ret := res.str()
	return ret
}

