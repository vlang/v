// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// SHA512 block step.
// This is the generic version with no architecture optimizations.
// In its own file so that an architecture
// optimized verision can be substituted
module sha512

import math.bits

const (
	_k = [u64(0x428a2f98d728ae22), u64(0x7137449123ef65cd), u64(0xb5c0fbcfec4d3b2f), u64(0xe9b5dba58189dbbc),
		u64(0x3956c25bf348b538), u64(0x59f111f1b605d019), u64(0x923f82a4af194f9b), u64(0xab1c5ed5da6d8118),
		u64(0xd807aa98a3030242), u64(0x12835b0145706fbe), u64(0x243185be4ee4b28c), u64(0x550c7dc3d5ffb4e2),
		u64(0x72be5d74f27b896f), u64(0x80deb1fe3b1696b1), u64(0x9bdc06a725c71235), u64(0xc19bf174cf692694),
		u64(0xe49b69c19ef14ad2), u64(0xefbe4786384f25e3), u64(0x0fc19dc68b8cd5b5), u64(0x240ca1cc77ac9c65),
		u64(0x2de92c6f592b0275), u64(0x4a7484aa6ea6e483), u64(0x5cb0a9dcbd41fbd4), u64(0x76f988da831153b5),
		u64(0x983e5152ee66dfab), u64(0xa831c66d2db43210), u64(0xb00327c898fb213f), u64(0xbf597fc7beef0ee4),
		u64(0xc6e00bf33da88fc2), u64(0xd5a79147930aa725), u64(0x06ca6351e003826f), u64(0x142929670a0e6e70),
		u64(0x27b70a8546d22ffc), u64(0x2e1b21385c26c926), u64(0x4d2c6dfc5ac42aed), u64(0x53380d139d95b3df),
		u64(0x650a73548baf63de), u64(0x766a0abb3c77b2a8), u64(0x81c2c92e47edaee6), u64(0x92722c851482353b),
		u64(0xa2bfe8a14cf10364), u64(0xa81a664bbc423001), u64(0xc24b8b70d0f89791), u64(0xc76c51a30654be30),
		u64(0xd192e819d6ef5218), u64(0xd69906245565a910), u64(0xf40e35855771202a), u64(0x106aa07032bbd1b8),
		u64(0x19a4c116b8d2d0c8), u64(0x1e376c085141ab53), u64(0x2748774cdf8eeb99), u64(0x34b0bcb5e19b48a8),
		u64(0x391c0cb3c5c95a63), u64(0x4ed8aa4ae3418acb), u64(0x5b9cca4f7763e373), u64(0x682e6ff3d6b2b8a3),
		u64(0x748f82ee5defb2fc), u64(0x78a5636f43172f60), u64(0x84c87814a1f0ab72), u64(0x8cc702081a6439ec),
		u64(0x90befffa23631e28), u64(0xa4506cebde82bde9), u64(0xbef9a3f7b2c67915), u64(0xc67178f2e372532b),
		u64(0xca273eceea26619c), u64(0xd186b8c721c0c207), u64(0xeada7dd6cde0eb1e), u64(0xf57d4f7fee6ed178),
		u64(0x06f067aa72176fba), u64(0x0a637dc5a2c898a6), u64(0x113f9804bef90dae), u64(0x1b710b35131c471b),
		u64(0x28db77f523047d84), u64(0x32caab7b40c72493), u64(0x3c9ebe0a15c9bebc), u64(0x431d67c49c100d4c),
		u64(0x4cc5d4becb3e42b6), u64(0x597f299cfc657e2a), u64(0x5fcb6fab3ad6faec), u64(0x6c44198c4a475817)]
)

fn block_generic(mut dig Digest, p_ []byte) {
	unsafe {
		mut p := p_
		mut w := []u64{len: (80)}
		mut h0 := dig.h[0]
		mut h1 := dig.h[1]
		mut h2 := dig.h[2]
		mut h3 := dig.h[3]
		mut h4 := dig.h[4]
		mut h5 := dig.h[5]
		mut h6 := dig.h[6]
		mut h7 := dig.h[7]
		for p.len >= chunk {
			for i in 0 .. 16 {
				j := i * 8
				w[i] = (u64(p[j]) << 56) |
					(u64(p[j + 1]) << 48) | (u64(p[j + 2]) << 40) |
					(u64(p[j + 3]) << 32) | (u64(p[j + 4]) << 24) |
					(u64(p[j + 5]) << 16) | (u64(p[j + 6]) << 8) | u64(p[j + 7])
			}
			for i := 16; i < 80; i++ {
				v1 := w[i - 2]
				t1 := bits.rotate_left_64(v1, -19) ^ bits.rotate_left_64(v1, -61) ^ (v1 >> 6)
				v2 := w[i - 15]
				t2 := bits.rotate_left_64(v2, -1) ^ bits.rotate_left_64(v2, -8) ^ (v2 >> 7)
				w[i] = t1 + w[i - 7] + t2 + w[i - 16]
			}
			mut a := h0
			mut b := h1
			mut c := h2
			mut d := h3
			mut e := h4
			mut f := h5
			mut g := h6
			mut h := h7
			for i in 0 .. 80 {
				t1 := h +
					(bits.rotate_left_64(e, -14) ^ bits.rotate_left_64(e, -18) ^ bits.rotate_left_64(e, -41)) +
					((e & f) ^ (~e & g)) + _k[i] + w[i]
				t2 := (bits.rotate_left_64(a, -28) ^ bits.rotate_left_64(a, -34) ^ bits.rotate_left_64(a, -39)) +
					((a & b) ^ (a & c) ^ (b & c))
				h = g
				g = f
				f = e
				e = d + t1
				d = c
				c = b
				b = a
				a = t1 + t2
			}
			h0 += a
			h1 += b
			h2 += c
			h3 += d
			h4 += e
			h5 += f
			h6 += g
			h7 += h
			if chunk >= p.len {
				p = []
			} else {
				p = p[chunk..]
			}
		}
		dig.h[0] = h0
		dig.h[1] = h1
		dig.h[2] = h2
		dig.h[3] = h3
		dig.h[4] = h4
		dig.h[5] = h5
		dig.h[6] = h6
		dig.h[7] = h7
	}
}
