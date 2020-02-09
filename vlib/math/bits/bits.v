// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

const (
// See http://supertech.csail.mit.edu/papers/debruijn.pdf
	de_bruijn32 = u32(0x077CB531)
	de_bruijn32tab = [byte(0), 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
	31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9,
	]
	de_bruijn64 = (0x03f79d71b4ca8b09)
	de_bruijn64tab = [byte(0), 1, 56, 2, 57, 49, 28, 3, 61, 58, 42, 50, 38, 29, 17, 4,
	62, 47, 59, 36, 45, 43, 51, 22, 53, 39, 33, 30, 24, 18, 12, 5,
	63, 55, 48, 27, 60, 41, 37, 16, 46, 35, 44, 21, 52, 32, 23, 11,
	54, 26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9, 13, 8, 7, 6,
	]
)

const (
	m0 = 0x5555555555555555 // 01010101 ...
	m1 = 0x3333333333333333 // 00110011 ...
	m2 = 0x0f0f0f0f0f0f0f0f // 00001111 ...
	m3 = 0x00ff00ff00ff00ff // etc.
	m4 = 0x0000ffff0000ffff
)

const (
	// save importing math mod just for these
	max_u32 = 4294967295
	max_u64 = 18446744073709551615
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
	return int(de_bruijn32tab[u32(x & -x) * de_bruijn32>>(32 - 5)])
}

// trailing_zeros_32 returns the number of trailing zero bits in x; the result is 32 for x == 0.
pub fn trailing_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	// see comment in trailing_zeros_64
	return int(de_bruijn32tab[(x & -x) * de_bruijn32>>(32 - 5)])
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
	return int(de_bruijn64tab[(x & -x) * de_bruijn64>>(64 - 6)])
}

// --- OnesCount ---
// ones_count_8 returns the number of one bits ("population count") in x.
pub fn ones_count_8(x byte) int {
	return int(pop_8_tab[x])
}

// ones_count_16 returns the number of one bits ("population count") in x.
pub fn ones_count_16(x u16) int {
	return int(pop_8_tab[x>>8] + pop_8_tab[x & u16(0xff)])
}

// ones_count_32 returns the number of one bits ("population count") in x.
pub fn ones_count_32(x u32) int {
	return int(pop_8_tab[x>>24] + pop_8_tab[x>>16 & 0xff] + pop_8_tab[x>>8 & 0xff] + pop_8_tab[x & u32(0xff)])
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
	mut y := (x>>u64(1) & (m0 & max_u64)) + (x & (m0 & max_u64))
	y = (y>>u64(2) & (m1 & max_u64)) + (y & (m1 & max_u64))
	y = ((y>>4) + y) & (m2 & max_u64)
	y += y>>8
	y += y>>16
	y += y>>32
	return int(y) & ((1<<7) - 1)
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
	return ((x<<s) | (x>>(n - s)))
}

// rotate_left_16 returns the value of x rotated left by (k mod 16) bits.
// To rotate x right by k bits, call rotate_left_16(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_16(x u16, k int) u16 {
	n := u16(16)
	s := u16(k) & (n - u16(1))
	return ((x<<s) | (x>>(n - s)))
}

// rotate_left_32 returns the value of x rotated left by (k mod 32) bits.
// To rotate x right by k bits, call rotate_left_32(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_32(x u32, k int) u32 {
	n := u32(32)
	s := u32(k) & (n - u32(1))
	return ((x<<s) | (x>>(n - s)))
}

// rotate_left_64 returns the value of x rotated left by (k mod 64) bits.
// To rotate x right by k bits, call rotate_left_64(x, -k).
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn rotate_left_64(x u64, k int) u64 {
	n := u64(64)
	s := u64(k) & (n - u64(1))
	return ((x<<s) | (x>>(n - s)))
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
	return u16(rev_8_tab[x>>8]) | (u16(rev_8_tab[x & u16(0xff)])<<8)
}

// reverse_32 returns the value of x with its bits in reversed order.
[inline]
pub fn reverse_32(x u32) u32 {
	mut y := (x>>u32(1) & (m0 & max_u32) | ((x & (m0 & max_u32))<<1))
	y = (y>>u32(2) & (m1 & max_u32) | ((y & (m1 & max_u32))<<u32(2)))
	y = (y>>u32(4) & (m2 & max_u32) | ((y & (m2 & max_u32))<<u32(4)))
	return reverse_bytes_32(y)
}

// reverse_64 returns the value of x with its bits in reversed order.
[inline]
pub fn reverse_64(x u64) u64 {
	mut y := (x>>u64(1) & (m0 & max_u64) | ((x & (m0 & max_u64))<<1))
	y = (y>>u64(2) & (m1 & max_u64) | ((y & (m1 & max_u64))<<2))
	y = (y>>u64(4) & (m2 & max_u64) | ((y & (m2 & max_u64))<<4))
	return reverse_bytes_64(y)
}

// --- ReverseBytes ---
// reverse_bytes_16 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn reverse_bytes_16(x u16) u16 {
	return (x>>8) | (x<<8)
}

// reverse_bytes_32 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn reverse_bytes_32(x u32) u32 {
	y := (x>>u32(8) & (m3 & max_u32) | ((x & (m3 & max_u32))<<u32(8)))
	return (y>>16) | (y<<16)
}

// reverse_bytes_64 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
[inline]
pub fn reverse_bytes_64(x u64) u64 {
	mut y := (x>>u64(8) & (m3 & max_u64) | ((x & (m3 & max_u64))<<u64(8)))
	y = (y>>u64(16) & (m4 & max_u64) | ((y & (m4 & max_u64))<<u64(16)))
	return (y>>32) | (y<<32)
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
	if y >= 1<<8 {
		y >>= 8
		n = 8
	}
	return n + int(len_8_tab[y])
}

// len_32 returns the minimum number of bits required to represent x; the result is 0 for x == 0.
pub fn len_32(x u32) int {
	mut y := x
	mut n := 0
	if y >= 1<<16 {
		y >>= 16
		n = 16
	}
	if y >= 1<<8 {
		y >>= 8
		n += 8
	}
	return n + int(len_8_tab[y])
}

// len_64 returns the minimum number of bits required to represent x; the result is 0 for x == 0.
pub fn len_64(x u64) int {
	mut y := x
	mut n := 0
	if y >= u64(1)<<u64(32) {
		y >>= 32
		n = 32
	}
	if y >= u64(1)<<u64(16) {
		y >>= 16
		n += 16
	}
	if y >= u64(1)<<u64(8) {
		y >>= 8
		n += 8
	}
	return n + int(len_8_tab[y])
}

