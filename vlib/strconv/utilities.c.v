module strconv

// import math

/*
f32/f64 to string utilities

Copyright (c) 2019-2021 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the f32/f64 to string utilities functions

These functions are based on the work of:
Publication:PLDI 2018: Proceedings of the 39th ACM SIGPLAN
Conference on Programming Language Design and ImplementationJune 2018
Pages 270–282 https://doi.org/10.1145/3192366.3192369

inspired by the Go version here:
https://github.com/cespare/ryu/tree/ba56a33f39e3bbbfa409095d0f9ae168a595feea
*/

/*
f64 to string with string format
*/

// TODO: Investigate precision issues
// f32_to_str_l return a string with the f32 converted in a string in decimal notation
[manualfree]
pub fn f32_to_str_l(f f32) string {
	s := f32_to_str(f, 6)
	res := fxx_to_str_l_parse(s)
	unsafe { s.free() }
	return res
}

[manualfree]
pub fn f32_to_str_l_no_dot(f f32) string {
	s := f32_to_str(f, 6)
	res := fxx_to_str_l_parse_no_dot(s)
	unsafe { s.free() }
	return res
}

[manualfree]
pub fn f64_to_str_l(f f64) string {
	s := f64_to_str(f, 18)
	res := fxx_to_str_l_parse(s)
	unsafe { s.free() }
	return res
}

[manualfree]
pub fn f64_to_str_l_no_dot(f f64) string {
	s := f64_to_str(f, 18)
	res := fxx_to_str_l_parse_no_dot(s)
	unsafe { s.free() }
	return res
}

// f64_to_str_l return a string with the f64 converted in a string in decimal notation
[manualfree]
pub fn fxx_to_str_l_parse(s string) string {
	// check for +inf -inf Nan
	if s.len > 2 && (s[0] == `n` || s[1] == `i`) {
		return s.clone()
	}

	m_sgn_flag := false
	mut sgn := 1
	mut b := [26]byte{}
	mut d_pos := 1
	mut i := 0
	mut i1 := 0
	mut exp := 0
	mut exp_sgn := 1

	// get sign and decimal parts
	for c in s {
		if c == `-` {
			sgn = -1
			i++
		} else if c == `+` {
			sgn = 1
			i++
		} else if c >= `0` && c <= `9` {
			b[i1] = c
			i1++
			i++
		} else if c == `.` {
			if sgn > 0 {
				d_pos = i
			} else {
				d_pos = i - 1
			}
			i++
		} else if c == `e` {
			i++
			break
		} else {
			return 'Float conversion error!!'
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
	mut res := []byte{len: exp + 32, init: 0}
	mut r_i := 0 // result string buffer index

	// println("s:${sgn} b:${b[0]} es:${exp_sgn} exp:${exp}")

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
	} else {
		mut dot_p := true
		for exp > 0 {
			res[r_i] = `0`
			r_i++
			exp--
			if dot_p {
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
	/*
	// remove the dot form the numbers like 2.
	if r_i > 1 && res[r_i-1] == `.` {
		r_i--
	}
	*/
	res[r_i] = 0
	return unsafe { tos(res.data, r_i) }
}

// f64_to_str_l return a string with the f64 converted in a string in decimal notation
[manualfree]
pub fn fxx_to_str_l_parse_no_dot(s string) string {
	// check for +inf -inf Nan
	if s.len > 2 && (s[0] == `n` || s[1] == `i`) {
		return s.clone()
	}

	m_sgn_flag := false
	mut sgn := 1
	mut b := [26]byte{}
	mut d_pos := 1
	mut i := 0
	mut i1 := 0
	mut exp := 0
	mut exp_sgn := 1

	// get sign and decimal parts
	for c in s {
		if c == `-` {
			sgn = -1
			i++
		} else if c == `+` {
			sgn = 1
			i++
		} else if c >= `0` && c <= `9` {
			b[i1] = c
			i1++
			i++
		} else if c == `.` {
			if sgn > 0 {
				d_pos = i
			} else {
				d_pos = i - 1
			}
			i++
		} else if c == `e` {
			i++
			break
		} else {
			return 'Float conversion error!!'
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
	mut res := []byte{len: exp + 32, init: 0}
	mut r_i := 0 // result string buffer index

	// println("s:${sgn} b:${b[0]} es:${exp_sgn} exp:${exp}")

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
	} else {
		mut dot_p := true
		for exp > 0 {
			res[r_i] = `0`
			r_i++
			exp--
			if dot_p {
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

	// remove the dot form the numbers like 2.
	if r_i > 1 && res[r_i - 1] == `.` {
		r_i--
	}

	res[r_i] = 0
	return unsafe { tos(res.data, r_i) }
}

// dec_digits return the number of decimal digit of an u64
pub fn dec_digits(n u64) int {
	if n <= 9_999_999_999 { // 1-10
		if n <= 99_999 { // 5
			if n <= 99 { // 2
				if n <= 9 { // 1
					return 1
				} else {
					return 2
				}
			} else {
				if n <= 999 { // 3
					return 3
				} else {
					if n <= 9999 { // 4
						return 4
					} else {
						return 5
					}
				}
			}
		} else {
			if n <= 9_999_999 { // 7
				if n <= 999_999 { // 6
					return 6
				} else {
					return 7
				}
			} else {
				if n <= 99_999_999 { // 8
					return 8
				} else {
					if n <= 999_999_999 { // 9
						return 9
					}
					return 10
				}
			}
		}
	} else {
		if n <= 999_999_999_999_999 { // 5
			if n <= 999_999_999_999 { // 2
				if n <= 99_999_999_999 { // 1
					return 11
				} else {
					return 12
				}
			} else {
				if n <= 9_999_999_999_999 { // 3
					return 13
				} else {
					if n <= 99_999_999_999_999 { // 4
						return 14
					} else {
						return 15
					}
				}
			}
		} else {
			if n <= 99_999_999_999_999_999 { // 7
				if n <= 9_999_999_999_999_999 { // 6
					return 16
				} else {
					return 17
				}
			} else {
				if n <= 999_999_999_999_999_999 { // 8
					return 18
				} else {
					if n <= 9_999_999_999_999_999_999 { // 9
						return 19
					}
					return 20
				}
			}
		}
	}
}
