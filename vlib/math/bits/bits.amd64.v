// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

fn C._umul128(x u64, y u64, result_hi &u64) u64
fn C._addcarry_u64(carry_in u8, a u64, b u64, out &u64) u8
fn C._udiv128(hi u64, lo u64, y u64, rem &u64) u64

@[inline]
pub fn mul_64(x u64, y u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	$if msvc {
		lo = C._umul128(x, y, &hi)
	} $else {
		asm amd64 {
			mulq rdx
			; =d (hi)
			  =a (lo)
			; a (x)
			  d (y)
			; cc
		}
	}
	return hi, lo
}

@[inline]
pub fn mul_add_64(x u64, y u64, z u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	$if msvc {
		lo = C._umul128(x, y, &hi)
		carry := C._addcarry_u64(0, lo, z, &lo)
		hi += carry
	} $else {
		asm amd64 {
			mulq rdx
			addq rax, z
			adcq rdx, 0
			; =d (hi)
			  =a (lo)
			; a (x)
			  d (y)
			  r (z)
			; cc
		}
	}
	return hi, lo
}

@[inline]
pub fn div_64(hi u64, lo u64, y1 u64) (u64, u64) {
	mut y := y1
	if y == 0 {
		panic(overflow_error)
	}
	if y <= hi {
		panic(overflow_error)
	}
	mut quo := u64(0)
	mut rem := u64(0)
	$if msvc {
		quo = C._udiv128(hi, lo, y, &rem)
	} $else {
		asm amd64 {
			div y
			; =a (quo)
			  =d (rem)
			; d (hi)
			  a (lo)
			  r (y)
			; cc
		}
	}
	return quo, rem
}
