module strconv

import strings

/*
printf/sprintf V implementation

Copyright (c) 2020 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the printf/sprintf functions
*/

// Align_text is used to describe the different ways to align a text - left, right and center
pub enum Align_text {
	right = 0
	left
	center
}

// Float conversion utility

// rounding value
const dec_round = [
	f64(0.5),
	0.05,
	0.005,
	0.0005,
	0.00005,
	0.000005,
	0.0000005,
	0.00000005,
	0.000000005,
	0.0000000005,
	0.00000000005,
	0.000000000005,
	0.0000000000005,
	0.00000000000005,
	0.000000000000005,
	0.0000000000000005,
	0.00000000000000005,
	0.000000000000000005,
	0.0000000000000000005,
	0.00000000000000000005,
	0.000000000000000000005,
	0.0000000000000000000005,
	0.00000000000000000000005,
	0.000000000000000000000005,
	0.0000000000000000000000005,
	0.00000000000000000000000005,
	0.000000000000000000000000005,
	0.0000000000000000000000000005,
	0.00000000000000000000000000005,
	0.000000000000000000000000000005,
	0.0000000000000000000000000000005,
	0.00000000000000000000000000000005,
	0.000000000000000000000000000000005,
	0.0000000000000000000000000000000005,
	0.00000000000000000000000000000000005,
	0.000000000000000000000000000000000005,
]!

// Single format functions

// BF_param is used for describing the formatting options for a single interpolated value
pub struct BF_param {
pub mut:
	pad_ch       u8   = u8(` `) // padding char
	len0         int  = -1      // default len for whole the number or string
	len1         int  = 6       // number of decimal digits, if needed
	positive     bool = true    // mandatory: the sign of the number passed
	sign_flag    bool // flag for print sign as prefix in padding
	align        Align_text = .right // alignment of the string
	rm_tail_zero bool // remove the tail zeros from floats
}

// format_str returns the `s` formatted, according to the options set in `p`.
@[manualfree]
pub fn format_str(s string, p BF_param) string {
	if p.len0 <= 0 {
		return s.clone()
	}
	dif := p.len0 - utf8_str_visible_length(s)
	if dif <= 0 {
		return s.clone()
	}
	mut res := strings.new_builder(s.len + dif)
	defer {
		unsafe { res.free() }
	}
	if p.align == .right {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	res.write_string(s)
	if p.align == .left {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	return res.str()
}
