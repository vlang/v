// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is the generic version with no architecture optimizations.
// In its own file so that an architecture
// optimized verision can be substituted

module md5

import math.bits

[direct_array_access; inline]
fn get_le_u32(b []u8, start int) u32 {
	return u32(b[start]) | (u32(b[1 + start]) << u32(8)) | (u32(b[2 + start]) << u32(16)) | (u32(b[
		3 + start]) << u32(24))
}

[direct_array_access]
fn block_generic(mut dig Digest, p []u8) {
	// load state
	mut a := dig.s[0]
	mut b := dig.s[1]
	mut c := dig.s[2]
	mut d := dig.s[3]

	for i := 0; i <= p.len - block_size; i += block_size {
		// save current state
		aa := a
		bb := b
		cc := c
		dd := d

		// load input block
		x0 := get_le_u32(p, 4 * 0x0 + i)
		x1 := get_le_u32(p, 4 * 0x1 + i)
		x2 := get_le_u32(p, 4 * 0x2 + i)
		x3 := get_le_u32(p, 4 * 0x3 + i)
		x4 := get_le_u32(p, 4 * 0x4 + i)
		x5 := get_le_u32(p, 4 * 0x5 + i)
		x6 := get_le_u32(p, 4 * 0x6 + i)
		x7 := get_le_u32(p, 4 * 0x7 + i)
		x8 := get_le_u32(p, 4 * 0x8 + i)
		x9 := get_le_u32(p, 4 * 0x9 + i)
		xa := get_le_u32(p, 4 * 0xa + i)
		xb := get_le_u32(p, 4 * 0xb + i)
		xc := get_le_u32(p, 4 * 0xc + i)
		xd := get_le_u32(p, 4 * 0xd + i)
		xe := get_le_u32(p, 4 * 0xe + i)
		xf := get_le_u32(p, 4 * 0xf + i)

		// round 1
		a = b + bits.rotate_left_32((((c ^ d) & b) ^ d) + a + x0 + u32(0xd76aa478), 7)
		d = a + bits.rotate_left_32((((b ^ c) & a) ^ c) + d + x1 + u32(0xe8c7b756), 12)
		c = d + bits.rotate_left_32((((a ^ b) & d) ^ b) + c + x2 + u32(0x242070db), 17)
		b = c + bits.rotate_left_32((((d ^ a) & c) ^ a) + b + x3 + u32(0xc1bdceee), 22)
		a = b + bits.rotate_left_32((((c ^ d) & b) ^ d) + a + x4 + u32(0xf57c0faf), 7)
		d = a + bits.rotate_left_32((((b ^ c) & a) ^ c) + d + x5 + u32(0x4787c62a), 12)
		c = d + bits.rotate_left_32((((a ^ b) & d) ^ b) + c + x6 + u32(0xa8304613), 17)
		b = c + bits.rotate_left_32((((d ^ a) & c) ^ a) + b + x7 + u32(0xfd469501), 22)
		a = b + bits.rotate_left_32((((c ^ d) & b) ^ d) + a + x8 + u32(0x698098d8), 7)
		d = a + bits.rotate_left_32((((b ^ c) & a) ^ c) + d + x9 + u32(0x8b44f7af), 12)
		c = d + bits.rotate_left_32((((a ^ b) & d) ^ b) + c + xa + u32(0xffff5bb1), 17)
		b = c + bits.rotate_left_32((((d ^ a) & c) ^ a) + b + xb + u32(0x895cd7be), 22)
		a = b + bits.rotate_left_32((((c ^ d) & b) ^ d) + a + xc + u32(0x6b901122), 7)
		d = a + bits.rotate_left_32((((b ^ c) & a) ^ c) + d + xd + u32(0xfd987193), 12)
		c = d + bits.rotate_left_32((((a ^ b) & d) ^ b) + c + xe + u32(0xa679438e), 17)
		b = c + bits.rotate_left_32((((d ^ a) & c) ^ a) + b + xf + u32(0x49b40821), 22)

		// round 2
		a = b + bits.rotate_left_32((((b ^ c) & d) ^ c) + a + x1 + u32(0xf61e2562), 5)
		d = a + bits.rotate_left_32((((a ^ b) & c) ^ b) + d + x6 + u32(0xc040b340), 9)
		c = d + bits.rotate_left_32((((d ^ a) & b) ^ a) + c + xb + u32(0x265e5a51), 14)
		b = c + bits.rotate_left_32((((c ^ d) & a) ^ d) + b + x0 + u32(0xe9b6c7aa), 20)
		a = b + bits.rotate_left_32((((b ^ c) & d) ^ c) + a + x5 + u32(0xd62f105d), 5)
		d = a + bits.rotate_left_32((((a ^ b) & c) ^ b) + d + xa + u32(0x02441453), 9)
		c = d + bits.rotate_left_32((((d ^ a) & b) ^ a) + c + xf + u32(0xd8a1e681), 14)
		b = c + bits.rotate_left_32((((c ^ d) & a) ^ d) + b + x4 + u32(0xe7d3fbc8), 20)
		a = b + bits.rotate_left_32((((b ^ c) & d) ^ c) + a + x9 + u32(0x21e1cde6), 5)
		d = a + bits.rotate_left_32((((a ^ b) & c) ^ b) + d + xe + u32(0xc33707d6), 9)
		c = d + bits.rotate_left_32((((d ^ a) & b) ^ a) + c + x3 + u32(0xf4d50d87), 14)
		b = c + bits.rotate_left_32((((c ^ d) & a) ^ d) + b + x8 + u32(0x455a14ed), 20)
		a = b + bits.rotate_left_32((((b ^ c) & d) ^ c) + a + xd + u32(0xa9e3e905), 5)
		d = a + bits.rotate_left_32((((a ^ b) & c) ^ b) + d + x2 + u32(0xfcefa3f8), 9)
		c = d + bits.rotate_left_32((((d ^ a) & b) ^ a) + c + x7 + u32(0x676f02d9), 14)
		b = c + bits.rotate_left_32((((c ^ d) & a) ^ d) + b + xc + u32(0x8d2a4c8a), 20)

		// round 3
		a = b + bits.rotate_left_32((b ^ c ^ d) + a + x5 + u32(0xfffa3942), 4)
		d = a + bits.rotate_left_32((a ^ b ^ c) + d + x8 + u32(0x8771f681), 11)
		c = d + bits.rotate_left_32((d ^ a ^ b) + c + xb + u32(0x6d9d6122), 16)
		b = c + bits.rotate_left_32((c ^ d ^ a) + b + xe + u32(0xfde5380c), 23)
		a = b + bits.rotate_left_32((b ^ c ^ d) + a + x1 + u32(0xa4beea44), 4)
		d = a + bits.rotate_left_32((a ^ b ^ c) + d + x4 + u32(0x4bdecfa9), 11)
		c = d + bits.rotate_left_32((d ^ a ^ b) + c + x7 + u32(0xf6bb4b60), 16)
		b = c + bits.rotate_left_32((c ^ d ^ a) + b + xa + u32(0xbebfbc70), 23)
		a = b + bits.rotate_left_32((b ^ c ^ d) + a + xd + u32(0x289b7ec6), 4)
		d = a + bits.rotate_left_32((a ^ b ^ c) + d + x0 + u32(0xeaa127fa), 11)
		c = d + bits.rotate_left_32((d ^ a ^ b) + c + x3 + u32(0xd4ef3085), 16)
		b = c + bits.rotate_left_32((c ^ d ^ a) + b + x6 + u32(0x04881d05), 23)
		a = b + bits.rotate_left_32((b ^ c ^ d) + a + x9 + u32(0xd9d4d039), 4)
		d = a + bits.rotate_left_32((a ^ b ^ c) + d + xc + u32(0xe6db99e5), 11)
		c = d + bits.rotate_left_32((d ^ a ^ b) + c + xf + u32(0x1fa27cf8), 16)
		b = c + bits.rotate_left_32((c ^ d ^ a) + b + x2 + u32(0xc4ac5665), 23)

		// round 4
		a = b + bits.rotate_left_32((c ^ (b | ~d)) + a + x0 + u32(0xf4292244), 6)
		d = a + bits.rotate_left_32((b ^ (a | ~c)) + d + x7 + u32(0x432aff97), 10)
		c = d + bits.rotate_left_32((a ^ (d | ~b)) + c + xe + u32(0xab9423a7), 15)
		b = c + bits.rotate_left_32((d ^ (c | ~a)) + b + x5 + u32(0xfc93a039), 21)
		a = b + bits.rotate_left_32((c ^ (b | ~d)) + a + xc + u32(0x655b59c3), 6)
		d = a + bits.rotate_left_32((b ^ (a | ~c)) + d + x3 + u32(0x8f0ccc92), 10)
		c = d + bits.rotate_left_32((a ^ (d | ~b)) + c + xa + u32(0xffeff47d), 15)
		b = c + bits.rotate_left_32((d ^ (c | ~a)) + b + x1 + u32(0x85845dd1), 21)
		a = b + bits.rotate_left_32((c ^ (b | ~d)) + a + x8 + u32(0x6fa87e4f), 6)
		d = a + bits.rotate_left_32((b ^ (a | ~c)) + d + xf + u32(0xfe2ce6e0), 10)
		c = d + bits.rotate_left_32((a ^ (d | ~b)) + c + x6 + u32(0xa3014314), 15)
		b = c + bits.rotate_left_32((d ^ (c | ~a)) + b + xd + u32(0x4e0811a1), 21)
		a = b + bits.rotate_left_32((c ^ (b | ~d)) + a + x4 + u32(0xf7537e82), 6)
		d = a + bits.rotate_left_32((b ^ (a | ~c)) + d + xb + u32(0xbd3af235), 10)
		c = d + bits.rotate_left_32((a ^ (d | ~b)) + c + x2 + u32(0x2ad7d2bb), 15)
		b = c + bits.rotate_left_32((d ^ (c | ~a)) + b + x9 + u32(0xeb86d391), 21)

		// add saved state
		a += aa
		b += bb
		c += cc
		d += dd
	}

	// save state
	dig.s[0] = a
	dig.s[1] = b
	dig.s[2] = c
	dig.s[3] = d
}
