// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

// mul_64 returns the 128-bit product of x and y: (hi, lo) = x * y
// with the product bits' upper half returned in hi and the lower
// half returned in lo.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn mul_64(x u64, y u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	asm arm64 {
		mul lo, x, y
		umulh hi, x, y
		; +r (hi)
		  +r (lo)
		; r (x)
		  r (y)
		; cc
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
	asm arm64 {
		mul lo, x, y
		umulh hi, x, y
		adds lo, lo, z
		adc hi, hi, xzr
		; +r (hi)
		  +r (lo)
		; r (x)
		  r (y)
		  r (z)
		; cc
	}
	return hi, lo
}
