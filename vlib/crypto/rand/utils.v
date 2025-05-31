// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

import math.bits
import math.big
import encoding.binary

// int_u64 returns a random unsigned 64-bit integer `u64` read from a real OS source of entropy.
pub fn int_u64(max u64) !u64 {
	bitlen := bits.len_64(max)
	if bitlen == 0 {
		return u64(0)
	}
	k := (bitlen + 7) / 8
	mut b := u64(bitlen % 8)
	if b == u64(0) {
		b = u64(8)
	}
	mut n := u64(0)
	for {
		mut bytes := read(k)!
		bytes[0] &= u8(int(u64(1) << b) - 1)
		x := bytes_to_u64(bytes)
		n = x[0]
		// NOTE: maybe until we have bigint could do it another way?
		// if x.len > 1 {
		// 	n = u64(u32(x[1])<<u32(32)) | n
		// }
		if n < max {
			return n
		}
	}
	return n
}

fn bytes_to_u64(b []u8) []u64 {
	ws := 64 / 8
	mut z := []u64{len: ((b.len + ws - 1) / ws)}
	mut i := b.len
	for k := 0; i >= ws; k++ {
		z[k] = binary.big_endian_u64(b[i - ws..i])
		i -= ws
	}
	if i > 0 {
		mut d := u64(0)
		for s := u64(0); i > 0; s += u64(8) {
			d |= u64(b[i - 1]) << s
			i--
		}
		z[z.len - 1] = d
	}
	return z
}

// int_big creates a random `big.Integer` with range [0, n)
// returns an error if `n` is 0 or negative.
pub fn int_big(n big.Integer) !big.Integer {
	if n.signum < 1 {
		return error('`n` cannot be 0 or negative.')
	}

	max := n - big.integer_from_int(1)
	len := max.bit_len()

	if len == 0 {
		// max = n - 1, if max = 0 then return max, as it is the only valid integer in [0, 1)
		return max
	}

	// k is the maximum byte length needed to encode a value < n
	k := (len + 7) / 8

	// b is the number of bits in the most significant byte of n-1
	mut b := u8(len % 8)
	if b == 0 {
		b = 8
	}

	mut result := big.Integer{}
	for {
		mut bytes := read(k)!

		// Clear bits in the first byte to increase the probability that the result is < max
		bytes[0] &= u8(int(1 << b) - 1)

		result = big.integer_from_bytes(bytes)
		if result < max {
			break
		}
	}
	return result
}
