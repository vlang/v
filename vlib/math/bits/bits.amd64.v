// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

fn C._umul128(x u64, y u64, result_hi &u64) u64
fn C._addcarry_u64(carry_in u8, a u64, b u64, out &u64) u8
fn C._udiv128(hi u64, lo u64, y u64, rem &u64) u64

// mul_64 returns the 128-bit product of x and y: (hi, lo) = x * y
// with the product bits' upper half returned in hi and the lower
// half returned in lo.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn mul_64(x u64, y u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	$if msvc {
		lo = C._umul128(x, y, &hi)
	} $else {
		asm amd64 {
			mov rax, x
			mov rdx, y
			mulq rdx
			mov lo, rax
			mov hi, rdx
			; +r (lo)
			  +r (hi)
			; r (x)
			  r (y)
			; rax
			  rdx
			  cc
		}
	}
	return hi, lo
}

// mul_add_64 returns the 128-bit result of x * y + z: (hi, lo) = x * y + z
// with the result bits' upper half returned in hi and the lower
// half returned in lo.
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
			mov rax, x
			mov rdx, y
			mulq rdx
			addq rax, z
			adcq rdx, 0
			mov lo, rax
			mov hi, rdx
			; +r (lo)
			  +r (hi)
			; r (x)
			  r (y)
			  r (z)
			; rax
			  rdx
			  cc
		}
	}
	return hi, lo
}

// div_64 returns the quotient and remainder of (hi, lo) divided by y:
// quo = (hi, lo)/y, rem = (hi, lo)%y with the dividend bits' upper
// half in parameter hi and the lower half in parameter lo.
// div_64 panics for y == 0 (division by zero) or y <= hi (quotient overflow).
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
			mov rax, lo
			mov rdx, hi
			div y
			mov quo, rax
			mov rem, rdx
			; +r (quo)
			  +r (rem)
			; r (hi)
			  r (lo)
			  r (y)
			; rax
			  rdx
			  cc
		}
	}
	return quo, rem
}
