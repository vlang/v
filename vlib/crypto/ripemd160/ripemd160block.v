// Based on: https://github.com/golang/crypto/blob/master/ripemd160/ripemd160block.go

// RIPEMD-160 block step.
module ripemd160

import math.bits

// vfmt off
const n__ = [
	u32(0), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
	7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
	3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
	1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
	4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13,
]

const r__ = [
	u32(11), 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
	7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
	11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
	11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
	9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6,
]

const n_ = [
	u32(5), 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
	6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
	15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
	8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
	12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11,
]

const r_ = [
	u32(8), 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
	9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
	9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
	15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
	8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11,
]
// vfmt on
@[direct_array_access]
fn block(mut md Digest, p0 []u8) int {
	mut p := p0.clone()
	mut n := 0
	mut x := []u32{len: 16}
	mut alpha := u32(0)
	mut beta := u32(0)

	for p.len >= block_size {
		mut a, mut b, mut c, mut d, mut e := md.s[0], md.s[1], md.s[2], md.s[3], md.s[4]
		mut aa, mut bb, mut cc, mut dd, mut ee := a, b, c, d, e
		mut j := 0

		for i := 0; i < 16; i++ {
			x[i] = u32(p[j]) | u32(p[j + 1]) << 8 | u32(p[j + 2]) << 16 | u32(p[j + 3]) << 24
			j += 4
		}

		mut i := 0
		for i < 16 {
			alpha = a + (b ^ c ^ d) + x[n__[i]]
			mut s := int(r__[i])
			alpha = bits.rotate_left_32(alpha, s) + e
			beta = bits.rotate_left_32(c, 10)
			a, b, c, d, e = e, alpha, b, beta, d

			alpha = aa + (bb ^ (cc | ~dd)) + x[n_[i]] + 0x50a28be6
			s = int(r_[i])
			alpha = bits.rotate_left_32(alpha, s) + ee
			beta = bits.rotate_left_32(cc, 10)
			aa, bb, cc, dd, ee = ee, alpha, bb, beta, dd

			i++
		}

		for i < 32 {
			alpha = a + (b & c | ~b & d) + x[n__[i]] + 0x5a827999
			mut s := int(r__[i])
			alpha = bits.rotate_left_32(alpha, s) + e
			beta = bits.rotate_left_32(c, 10)
			a, b, c, d, e = e, alpha, b, beta, d

			// parallel line
			alpha = aa + (bb & dd | cc & ~dd) + x[n_[i]] + 0x5c4dd124
			s = int(r_[i])
			alpha = bits.rotate_left_32(alpha, s) + ee
			beta = bits.rotate_left_32(cc, 10)
			aa, bb, cc, dd, ee = ee, alpha, bb, beta, dd

			i++
		}

		for i < 48 {
			alpha = a + (b | ~c ^ d) + x[n__[i]] + 0x6ed9eba1
			mut s := int(r__[i])
			alpha = bits.rotate_left_32(alpha, s) + e
			beta = bits.rotate_left_32(c, 10)
			a, b, c, d, e = e, alpha, b, beta, d

			// parallel line
			alpha = aa + (bb | ~cc ^ dd) + x[n_[i]] + 0x6d703ef3
			s = int(r_[i])
			alpha = bits.rotate_left_32(alpha, s) + ee
			beta = bits.rotate_left_32(cc, 10)
			aa, bb, cc, dd, ee = ee, alpha, bb, beta, dd

			i++
		}

		for i < 64 {
			alpha = a + (b & d | c & ~d) + x[n__[i]] + 0x8f1bbcdc
			mut s := int(r__[i])
			alpha = bits.rotate_left_32(alpha, s) + e
			beta = bits.rotate_left_32(c, 10)
			a, b, c, d, e = e, alpha, b, beta, d

			// parallel line
			alpha = aa + (bb & cc | ~bb & dd) + x[n_[i]] + 0x7a6d76e9
			s = int(r_[i])
			alpha = bits.rotate_left_32(alpha, s) + ee
			beta = bits.rotate_left_32(cc, 10)
			aa, bb, cc, dd, ee = ee, alpha, bb, beta, dd

			i++
		}

		for i < 80 {
			alpha = a + (b ^ (c | ~d)) + x[n__[i]] + 0xa953fd4e
			mut s := int(r__[i])
			alpha = bits.rotate_left_32(alpha, s) + e
			beta = bits.rotate_left_32(c, 10)
			a, b, c, d, e = e, alpha, b, beta, d

			// parallel line
			alpha = aa + (bb ^ cc ^ dd) + x[n_[i]]
			s = int(r_[i])
			alpha = bits.rotate_left_32(alpha, s) + ee
			beta = bits.rotate_left_32(cc, 10)
			aa, bb, cc, dd, ee = ee, alpha, bb, beta, dd

			i++
		}

		dd += c + md.s[1]
		md.s[1] = md.s[2] + d + ee
		md.s[2] = md.s[3] + e + aa
		md.s[3] = md.s[4] + a + bb
		md.s[4] = md.s[0] + b + cc
		md.s[0] = dd

		p = p[block_size..].clone()
		n += block_size
	}
	return n
}
