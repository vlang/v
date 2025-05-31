/**********************************************************************
* regex samples
*
* Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* This file contains a collection of regex samples
*
**********************************************************************/
import regex

/*
This simple function converts an HTML RGB value with 3 or 6 hex digits to a u32 value,
this function is not optimized and it is only for didatical purpose
example: #A0B0CC #A9F
*/
fn convert_html_rgb(in_col string) u32 {
	mut n_digit := if in_col.len == 4 { 1 } else { 2 }
	mut col_mul := if in_col.len == 4 { 4 } else { 0 }

	// this is the regex query, it uses V string interpolation to customize the regex query
	// NOTE: if you want use escaped code you must use the r"" (raw) strings,
	//       *** please remember that V interpoaltion doesn't work on raw strings. ***

	query := '#([a-fA-F0-9]{${n_digit}})([a-fA-F0-9]{${n_digit}})([a-fA-F0-9]{${n_digit}})'

	mut re := regex.regex_opt(query) or { panic(err) }
	start, end := re.match_string(in_col)
	println('start: ${start}, end: ${end}')
	mut res := u32(0)
	if start >= 0 {
		group_list := re.get_group_list()
		r := ('0x' + in_col[group_list[0].start..group_list[0].end]).u32() << col_mul
		g := ('0x' + in_col[group_list[1].start..group_list[1].end]).u32() << col_mul
		b := ('0x' + in_col[group_list[2].start..group_list[2].end]).u32() << col_mul
		println('r: ${r} g: ${g} b: ${b}')
		res = r << 16 | g << 8 | b
	}
	return res
}

/*
This function demonstrates the use of the named groups
*/
fn convert_html_rgb_n(in_col string) u32 {
	mut n_digit := if in_col.len == 4 { 1 } else { 2 }
	mut col_mul := if in_col.len == 4 { 4 } else { 0 }

	query := '#(?P<red>[a-fA-F0-9]{${n_digit}})(?P<green>[a-fA-F0-9]{${n_digit}})(?P<blue>[a-fA-F0-9]{${n_digit}})'

	mut re := regex.regex_opt(query) or { panic(err) }
	start, end := re.match_string(in_col)
	println('start: ${start}, end: ${end}')
	mut res := u32(0)
	if start >= 0 {
		red_s, red_e := re.get_group_bounds_by_name('red')
		r := ('0x' + in_col[red_s..red_e]).u32() << col_mul

		green_s, green_e := re.get_group_bounds_by_name('green')
		g := ('0x' + in_col[green_s..green_e]).u32() << col_mul

		blue_s, blue_e := re.get_group_bounds_by_name('blue')
		b := ('0x' + in_col[blue_s..blue_e]).u32() << col_mul

		println('r: ${r} g: ${g} b: ${b}')
		res = r << 16 | g << 8 | b
	}
	return res
}

fn main() {
	// convert HTML rgb color using groups
	println(convert_html_rgb('#A0b0Cc').hex())
	println(convert_html_rgb('#ABC').hex())

	// convert HTML rgb color using named groups
	println(convert_html_rgb_n('#A0B0CC').hex())
	println(convert_html_rgb_n('#ABC').hex())
}
