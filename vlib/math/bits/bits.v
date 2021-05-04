// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

const (
	// See http://supertech.csail.mit.edu/papers/debruijn.pdf
	de_bruijn32    = u32(0x077CB531)
	de_bruijn32tab = [byte(0), 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 31, 27, 13,
		23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9]
	de_bruijn64    = u64(0x03f79d71b4ca8b09)
	de_bruijn64tab = [byte(0), 1, 56, 2, 57, 49, 28, 3, 61, 58, 42, 50, 38, 29, 17, 4, 62, 47,
		59, 36, 45, 43, 51, 22, 53, 39, 33, 30, 24, 18, 12, 5, 63, 55, 48, 27, 60, 41, 37, 16,
		46, 35, 44, 21, 52, 32, 23, 11, 54, 26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9, 13, 8, 7,
		6,
	]
)

const (
	m0 = u64(0x5555555555555555) // 01010101 ...
	m1 = u64(0x3333333333333333) // 00110011 ...
	m2 = u64(0x0f0f0f0f0f0f0f0f) // 00001111 ...
	m3 = u64(0x00ff00ff00ff00ff) // etc.
	m4 = u64(0x0000ffff0000ffff)
)

const (
	// save importing math mod just for these
	max_u32 = u32(4294967295)
	max_u64 = u64(18446744073709551615)
)

// --- LeadingZeros ---
// leading_zeros_8 returns the number of leading zero bits in x; the result is 8 for x == 0.
pub fn leading_zeros_8(x byte) int {
	return 8 - len_8(x)
}

// leading_zeros_16 returns the number of leading zero bits in x; the result is 16 for x == 0.
pub fn leading_zeros_16(x u16) int {
	return 16 - len_16(x)
}

// leading_zeros_32 returns the number of leading zero bits in x; the result is 32 for x == 0.
pub fn leading_zeros_32(x u32) int {
	return 32 - len_32(x)
}

// leading_zeros_64 returns the number of leading zero bits in x; the result is 64 for x == 0.
pub fn leading_zeros_64(x u64) int {
	return 64 - len_64(x)
}

// --- TrailingZeros ---
// trailing_zeros_8 returns the number of trailing zero bits in x; the result is 8 for x == 0.
pub fn trailing_zeros_8(x byte) int {
	return int(ntz_8_tab[x])
}

// trailing_zeros_16 returns the number of trailing zero bits in x; the result is 16 for x == 0.
pub fn trailing_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	// see comment in trailing_zeros_64
	return int(bits.de_bruijn32tab[u32(x & -x) * bits.de_bruijn32 >> (32 - 5)])
}

// trailing_zeros_32 returns the number of trailing zero bits in x; the result is 32 for x == 0.
pub fn trailing_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	// see comment in trailing_zeros_64
	return int(bits.de_bruijn32tab[(x & -x) * bits.de_bruijn32 >> (32 - 5)])
}

// trailing_zeros_64 returns the number of trailing zero bits in x; the result is 64 for x == 0.
pub fn trailing_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	// If popcount is fast, replace code below with return popcount(^x & (x - 1)).
	//
	// x & -x leaves only the right-most bit set in the word. Let k be the
	// index of that bit. Since only a single bit is set, the value is two
	// to the power of k. Multiplying by a power of two is equivalent to
	// left shifting, in this case by k bits. The de Bruijn (64 bit) constant
	// is such that all six bit, consecutive substrings are distinct.
	// Therefore, if we have a left shifted version of this constant we can
	// find by how many bits it was shifted by looking at which six bit
	// substring ended up at the top of the word.
	// (Knuth, volume 4, section 7.3.1)
	return int(bits.de_bruijn64tab[(x & -x) * bits.de_bruijn64 >> (64 - 6)])
}

// --- OnesCount ---
// ones_count_8 returns the number of one bits ("population count") in x.
pub fn ones_count_8(x byte) int {
	return int(pop_8_tab[x])
}

// ones_count_16 returns the number of one bits ("population count") in x.
pub fn ones_count_16(x u16) int {
	return int(pop_8_tab[x >> 8] + pop_8_tab[x & u16(0xff)])
}

// ones_count_32 returns the number of one bits ("population count") in x.
pub fn ones_count_32(x u32) int {
	return int(pop_8_tab[x >> 24] + pop_8_tab[x >> 16 & 0xff] + pop_8_tab[x >> 8 & 0xff] +
		pop_8_tab[x & u32(0xff)])
}

// ones_count_64 returns the number of one bits ("population count") in x.
pub fn ones_count_64(x u64) int {
	// Implementation: Parallel summing of adjacent bits.
	// See "Hacker's Delight", Chap. 5: Counting Bits.
	// The following pattern shows the general approach:
	//
	// x = x>>1&(m0&m) + x&(m0&m)
	// x = x>>2&(m1&m) + x&(m1&m)
	// x = x>>4&(m2&m) + x&(m2&m)
	// x = x>>8&(m3&m) + x&(m3&m)
	// x = x>>16&(m4&m) + x&(m4&m)
	// x = x>>32&(m5&m) + x&(m5&m)
	// return int(x)
	//
	// Masking (& operations) can be left away when there's no
	// danger that a field's sum will carry over into the next
	// field: Since the result cannot be > 64, 8 bits is enough
	// and we can ignore the masks for the shifts by 8 and up.
	// Per "Hacker's Delight", the first line can be simplified
	// more, but it saves at best one instruction, so we leave
	// it alone for clarity.
	mut y := (x >> u64(1) & (bits.m0 & bits.max_u64)) + (x & (bits.m0 & bits.max_u64))
	y = (y >> u64(2) & (bits.m1 & bits.max_u64)) + (y & (bits.m1 & bits.max_u64))
	y = ((y >> 4) + y) & (bits.m2 & bits.max_u64)
	y += y >> 8
	y += y >> 16
	y += y >> 32
	return int(y) & ((1 << 7) - 1)
}

// --- RotateLeft ---
// rotate_left_8 returns the value of x rotated left by (k mod 8) bits.
// To rotate x right by k bits, call rotate_left_8(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_8(x byte, k int) byte {
	n := byte(8)
	s := byte(k) & (n - byte(1))
	return ((x << s) | (x >> (n - s)))
}

// rotate_left_16 returns the value of x rotated left by (k mod 16) bits.
// To rotate x right by k bits, call rotate_left_16(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_16(x u16, k int) u16 {
	n := u16(16)
	s := u16(k) & (n - u16(1))
	return ((x << s) | (x >> (n - s)))
}

// rotate_left_32 returns the value of x rotated left by (k mod 32) bits.
// To rotate x right by k bits, call rotate_left_32(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_32(x u32, k int) u32 {
	n := u32(32)
	s := u32(k) & (n - u32(1))
	return ((x << s) | (x >> (n - s)))
}

// rotate_left_64 returns the value of x rotated left by (k mod 64) bits.
// To rotate x right by k bits, call rotate_left_64(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_64(x u64, k int) u64 {
	n := u64(64)
	s := u64(k) & (n - u64(1))
	return ((x << s) | (x >> (n - s)))
}

// --- Reverse ---
// reverse_8 returns the value of x with its bits in reversed order.
[inline]
pub fn reverse_8(x byte) byte {
	return rev_8_tab[x]
}

// reverse_16 returns the value of x with its bits in reversed order.
[inline]
pub fn reverse_16(x u16) u16 {
	return u16(rev_8_tab[x >> 8]) | (u16(rev_8_tab[x & u16(0xff)]) << 8)
}

// reverse_32 returns the value of x with its bits in reversed order.
[inline]
pub fn reverse_32(x u32) u32 {
	mut y := ((x >> u32(1) & (bits.m0 & bits.max_u32)) | ((x & (bits.m0 & bits.max_u32)) << 1))
	y = ((y >> u32(2) & (bits.m1 & bits.max_u32)) | ((y & (bits.m1 & bits.max_u32)) << u32(2)))
	y = ((y >> u32(4) & (bits.m2 & bits.max_u32)) | ((y & (bits.m2 & bits.max_u32)) << u32(4)))
	return reverse_bytes_32(u32(y))
}

// reverse_64 returns the value of x with its bits in reversed order.
[inline]
pub fn reverse_64(x u64) u64 {
	mut y := ((x >> u64(1) & (bits.m0 & bits.max_u64)) | ((x & (bits.m0 & bits.max_u64)) << 1))
	y = ((y >> u64(2) & (bits.m1 & bits.max_u64)) | ((y & (bits.m1 & bits.max_u64)) << 2))
	y = ((y >> u64(4) & (bits.m2 & bits.max_u64)) | ((y & (bits.m2 & bits.max_u64)) << 4))
	return reverse_bytes_64(y)
}

// --- ReverseBytes ---
// reverse_bytes_16 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn reverse_bytes_16(x u16) u16 {
	return (x >> 8) | (x << 8)
}

// reverse_bytes_32 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn reverse_bytes_32(x u32) u32 {
	y := ((x >> u32(8) & (bits.m3 & bits.max_u32)) | ((x & (bits.m3 & bits.max_u32)) << u32(8)))
	return u32((y >> 16) | (y << 16))
}

// reverse_bytes_64 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn reverse_bytes_64(x u64) u64 {
	mut y := ((x >> u64(8) & (bits.m3 & bits.max_u64)) | ((x & (bits.m3 & bits.max_u64)) << u64(8)))
	y = ((y >> u64(16) & (bits.m4 & bits.max_u64)) | ((y & (bits.m4 & bits.max_u64)) << u64(16)))
	return (y >> 32) | (y << 32)
}

// --- Len ---
// len_8 returns the minimum number of bits required to represent x; the result is 0 for x == 0.
pub fn len_8(x byte) int {
	return int(len_8_tab[x])
}

// len_16 returns the minimum number of bits required to represent x; the result is 0 for x == 0.
pub fn len_16(x u16) int {
	mut y := x
	mut n := 0
	if y >= 1 << 8 {
		y >>= 8
		n = 8
	}
	return n + int(len_8_tab[y])
}

// len_32 returns the minimum number of bits required to represent x; the result is 0 for x == 0.
pub fn len_32(x u32) int {
	mut y := x
	mut n := 0
	if y >= (1 << 16) {
		y >>= 16
		n = 16
	}
	if y >= (1 << 8) {
		y >>= 8
		n += 8
	}
	return n + int(len_8_tab[y])
}

// len_64 returns the minimum number of bits required to represent x; the result is 0 for x == 0.
pub fn len_64(x u64) int {
	mut y := x
	mut n := 0
	if y >= u64(1) << u64(32) {
		y >>= 32
		n = 32
	}
	if y >= u64(1) << u64(16) {
		y >>= 16
		n += 16
	}
	if y >= u64(1) << u64(8) {
		y >>= 8
		n += 8
	}
	return n + int(len_8_tab[y])
}

// --- Add with carry ---
// Add returns the sum with carry of x, y and carry: sum = x + y + carry.
// The carry input must be 0 or 1; otherwise the behavior is undefined.
// The carryOut output is guaranteed to be 0 or 1.
//
// add_32 returns the sum with carry of x, y and carry: sum = x + y + carry.
// The carry input must be 0 or 1; otherwise the behavior is undefined.
// The carryOut output is guaranteed to be 0 or 1.
//
// This function's execution time does not depend on the inputs.
pub fn add_32(x u32, y u32, carry u32) (u32, u32) {
	sum64 := u64(x) + u64(y) + u64(carry)
	sum := u32(sum64)
	carry_out := u32(sum64 >> 32)
	return sum, carry_out
}

// add_64 returns the sum with carry of x, y and carry: sum = x + y + carry.
// The carry input must be 0 or 1; otherwise the behavior is undefined.
// The carryOut output is guaranteed to be 0 or 1.
//
// This function's execution time does not depend on the inputs.
pub fn add_64(x u64, y u64, carry u64) (u64, u64) {
	sum := x + y + carry
	// The sum will overflow if both top bits are set (x & y) or if one of them
	// is (x | y), and a carry from the lower place happened. If such a carry
	// happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
	carry_out := ((x & y) | ((x | y) & ~sum)) >> 63
	return sum, carry_out
}

// --- Subtract with borrow ---
// Sub returns the difference of x, y and borrow: diff = x - y - borrow.
// The borrow input must be 0 or 1; otherwise the behavior is undefined.
// The borrowOut output is guaranteed to be 0 or 1.
//
// sub_32 returns the difference of x, y and borrow, diff = x - y - borrow.
// The borrow input must be 0 or 1; otherwise the behavior is undefined.
// The borrowOut output is guaranteed to be 0 or 1.
//
// This function's execution time does not depend on the inputs.
pub fn sub_32(x u32, y u32, borrow u32) (u32, u32) {
	diff := x - y - borrow
	// The difference will underflow if the top bit of x is not set and the top
	// bit of y is set (^x & y) or if they are the same (^(x ^ y)) and a borrow
	// from the lower place happens. If that borrow happens, the result will be
	// 1 - 1 - 1 = 0 - 0 - 1 = 1 (& diff).
	borrow_out := ((~x & y) | (~(x ^ y) & diff)) >> 31
	return diff, borrow_out
}

// sub_64 returns the difference of x, y and borrow: diff = x - y - borrow.
// The borrow input must be 0 or 1; otherwise the behavior is undefined.
// The borrowOut output is guaranteed to be 0 or 1.
//
// This function's execution time does not depend on the inputs.
pub fn sub_64(x u64, y u64, borrow u64) (u64, u64) {
	diff := x - y - borrow
	// See Sub32 for the bit logic.
	borrow_out := ((~x & y) | (~(x ^ y) & diff)) >> 63
	return diff, borrow_out
}

// --- Full-width multiply ---
const (
	two32          = u64(0x100000000)
	mask32         = two32 - 1
	overflow_error = 'Overflow Error'
	divide_error   = 'Divide Error'
)

// mul_32 returns the 64-bit product of x and y: (hi, lo) = x * y
// with the product bits' upper half returned in hi and the lower
// half returned in lo.
//
// This function's execution time does not depend on the inputs.
pub fn mul_32(x u32, y u32) (u32, u32) {
	tmp := u64(x) * u64(y)
	hi := u32(tmp >> 32)
	lo := u32(tmp)
	return hi, lo
}

// mul_64 returns the 128-bit product of x and y: (hi, lo) = x * y
// with the product bits' upper half returned in hi and the lower
// half returned in lo.
//
// This function's execution time does not depend on the inputs.
pub fn mul_64(x u64, y u64) (u64, u64) {
	x0 := x & bits.mask32
	x1 := x >> 32
	y0 := y & bits.mask32
	y1 := y >> 32
	w0 := x0 * y0
	t := x1 * y0 + (w0 >> 32)
	mut w1 := t & bits.mask32
	w2 := t >> 32
	w1 += x0 * y1
	hi := x1 * y1 + w2 + (w1 >> 32)
	lo := x * y
	return hi, lo
}

// --- Full-width divide ---
// div_32 returns the quotient and remainder of (hi, lo) divided by y:
// quo = (hi, lo)/y, rem = (hi, lo)%y with the dividend bits' upper
// half in parameter hi and the lower half in parameter lo.
// div_32 panics for y == 0 (division by zero) or y <= hi (quotient overflow).
pub fn div_32(hi u32, lo u32, y u32) (u32, u32) {
	if y != 0 && y <= hi {
		panic(bits.overflow_error)
	}
	z := (u64(hi) << 32) | u64(lo)
	quo := u32(z / u64(y))
	rem := u32(z % u64(y))
	return quo, rem
}

// div_64 returns the quotient and remainder of (hi, lo) divided by y:
// quo = (hi, lo)/y, rem = (hi, lo)%y with the dividend bits' upper
// half in parameter hi and the lower half in parameter lo.
// div_64 panics for y == 0 (division by zero) or y <= hi (quotient overflow).
pub fn div_64(hi u64, lo u64, y1 u64) (u64, u64) {
	mut y := y1
	if y == 0 {
		panic(bits.overflow_error)
	}
	if y <= hi {
		panic(bits.overflow_error)
	}
	s := u32(leading_zeros_64(y))
	y <<= s
	yn1 := y >> 32
	yn0 := y & bits.mask32
	un32 := (hi << s) | (lo >> (64 - s))
	un10 := lo << s
	un1 := un10 >> 32
	un0 := un10 & bits.mask32
	mut q1 := un32 / yn1
	mut rhat := un32 - q1 * yn1
	for q1 >= bits.two32 || q1 * yn0 > bits.two32 * rhat + un1 {
		q1--
		rhat += yn1
		if rhat >= bits.two32 {
			break
		}
	}
	un21 := un32 * bits.two32 + un1 - q1 * y
	mut q0 := un21 / yn1
	rhat = un21 - q0 * yn1
	for q0 >= bits.two32 || q0 * yn0 > bits.two32 * rhat + un0 {
		q0--
		rhat += yn1
		if rhat >= bits.two32 {
			break
		}
	}
	return q1 * bits.two32 + q0, (un21 * bits.two32 + un0 - q0 * y) >> s
}

// rem_32 returns the remainder of (hi, lo) divided by y. Rem32 panics
// for y == 0 (division by zero) but, unlike Div32, it doesn't panic
// on a quotient overflow.
pub fn rem_32(hi u32, lo u32, y u32) u32 {
	return u32(((u64(hi) << 32) | u64(lo)) % u64(y))
}

// rem_64 returns the remainder of (hi, lo) divided by y. Rem64 panics
// for y == 0 (division by zero) but, unlike div_64, it doesn't panic
// on a quotient overflow.
pub fn rem_64(hi u64, lo u64, y u64) u64 {
	// We scale down hi so that hi < y, then use div_64 to compute the
	// rem with the guarantee that it won't panic on quotient overflow.
	// Given that
	// hi ≡ hi%y    (mod y)
	// we have
	// hi<<64 + lo ≡ (hi%y)<<64 + lo    (mod y)
	_, rem := div_64(hi % y, lo, y)
	return rem
}
