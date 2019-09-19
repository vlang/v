// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

#include <float.h>
#include <math.h>

pub fn (d double) str() string {
	return '0'
}

pub fn (d f64) str() string {
	return '0'
}

pub fn (d f32) str() string {
	return '0'
}

pub fn ptr_str(ptr voidptr) string {
	return '0'
}

// compare floats using C epsilon
pub fn (a f64) eq(b f64) bool {
	//return C.fabs(a - b) <= C.DBL_EPSILON	
	return (a - b) <= 0.01
}

// fn (nn i32) str() string {
// return i
// }
pub fn (nn int) str() string {
	return '0'
}

pub fn (nn u32) str() string {
	return '0'
}

pub fn (nn u8) str() string {
	return '0'
}

pub fn (nn i64) str() string {
	return '0'
}

pub fn (nn u64) str() string {
	return '0'
}

pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

pub fn (n int) hex() string {
	return '0'
}

pub fn (n i64) hex() string {
	return '0'
}

pub fn (a []byte) contains(val byte) bool {
	for aa in a {
		if aa == val {
			return true
		}
	}
	return false
}

pub fn (c rune) str() string {
	return '0'
}

pub fn (c byte) str() string {
	return '0'
}

pub fn (c byte) is_capital() bool {
	return c >= `A` && c <= `Z`
}

pub fn (b []byte) clone() []byte {
	mut res := [byte(0)].repeat(b.len)
	for i := 0; i < b.len; i++ {
		res[i] = b[i]
	}
	return res
}


