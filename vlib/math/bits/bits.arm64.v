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
	$if arm64 && !tinyc {
		asm arm64 {
			mul lo, x, y
			umulh hi, x, y
			; =&r (hi)
			  =&r (lo)
			; r (x)
			  r (y)
			; cc
		}
		return hi, lo
	}
	// cross compile
	return mul_64_default(x, y)
}

// mul_add_64 returns the 128-bit result of x * y + z: (hi, lo) = x * y + z
// with the result bits' upper half returned in hi and the lower
// half returned in lo.
@[inline]
pub fn mul_add_64(x u64, y u64, z u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	$if arm64 && !tinyc {
		asm arm64 {
			mul lo, x, y
			umulh hi, x, y
			adds lo, lo, z
			adc hi, hi, xzr
			; =&r (hi)
			  =&r (lo)
			; r (x)
			  r (y)
			  r (z)
			; cc
		}
		return hi, lo
	}
	// cross compile
	return mul_add_64_default(x, y, z)
}

// leading_zeros_8 returns the number of leading zero bits in x; the result is 8 for x == 0.
@[inline]
pub fn leading_zeros_8(x u8) int {
	if x == 0 {
		return 8
	}
	mut n := u32(x)
	asm arm64 {
		clz n, n
		; +r (n)
	}
	return int(n) - 24
}

// leading_zeros_16 returns the number of leading zero bits in x; the result is 16 for x == 0.
@[inline]
pub fn leading_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	mut n := u32(x)
	asm arm64 {
		clz n, n
		; +r (n)
	}
	return int(n) - 16
}

// leading_zeros_32 returns the number of leading zero bits in x; the result is 32 for x == 0.
@[inline]
pub fn leading_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	mut n := x
	asm arm64 {
		clz n, n
		; +r (n)
	}
	return int(n)
}

// leading_zeros_64 returns the number of leading zero bits in x; the result is 64 for x == 0.
@[inline]
pub fn leading_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	mut n := x
	asm arm64 {
		clz n, n
		; +r (n)
	}
	return int(n)
}

// trailing_zeros_8 returns the number of trailing zero bits in x; the result is 8 for x == 0.
@[inline]
pub fn trailing_zeros_8(x u8) int {
	if x == 0 {
		return 8
	}
	mut n := u32(x)
	asm arm64 {
		rbit n, n
		clz n, n
		; +r (n)
	}
	return int(n)
}

// trailing_zeros_16 returns the number of trailing zero bits in x; the result is 16 for x == 0.
@[inline]
pub fn trailing_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	mut n := u32(x)
	asm arm64 {
		rbit n, n
		clz n, n
		; +r (n)
	}
	return int(n)
}

// trailing_zeros_32 returns the number of trailing zero bits in x; the result is 32 for x == 0.
@[inline]
pub fn trailing_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	mut n := x
	asm arm64 {
		rbit n, n
		clz n, n
		; +r (n)
	}
	return int(n)
}

// trailing_zeros_64 returns the number of trailing zero bits in x; the result is 64 for x == 0.
@[inline]
pub fn trailing_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	mut n := x
	asm arm64 {
		rbit n, n
		clz n, n
		; +r (n)
	}
	return int(n)
}

// reverse_bytes_16 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn reverse_bytes_16(x u16) u16 {
	mut n := u32(x)
	asm arm64 {
		rev16 n, n
		; +r (n)
	}
	return u16(n)
}

// reverse_bytes_32 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn reverse_bytes_32(x u32) u32 {
	mut n := x
	asm arm64 {
		rev n, n
		; +r (n)
	}
	return n
}

// reverse_bytes_64 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn reverse_bytes_64(x u64) u64 {
	mut n := x
	asm arm64 {
		rev n, n
		; +r (n)
	}
	return n
}
