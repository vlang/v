module strconv

/*
printf/sprintf V implementation

Copyright (c) 2020 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the printf/sprintf functions
*/
import strings

pub enum Align_text {
	right = 0
	left
	center
}

/*
Float conversion utility
*/
const (
	// rounding value
	dec_round = [
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
	]
)

/*
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
*/
// max float 1.797693134862315708145274237317043567981e+308

/*
Single format functions
*/
pub struct BF_param {
pub mut:
	pad_ch       byte = byte(` `) // padding char
	len0         int  = -1 // default len for whole the number or string
	len1         int  = 6 // number of decimal digits, if needed
	positive     bool = true // mandatory: the sign of the number passed
	sign_flag    bool       // flag for print sign as prefix in padding
	allign       Align_text = .right // alignment of the string
	rm_tail_zero bool       // remove the tail zeros from floats
}

[manualfree]
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
	if p.allign == .right {
		for i1 := 0; i1 < dif; i1++ {
			res.write_byte(p.pad_ch)
		}
	}
	res.write_string(s)
	if p.allign == .left {
		for i1 := 0; i1 < dif; i1++ {
			res.write_byte(p.pad_ch)
		}
	}
	return res.str()
}
