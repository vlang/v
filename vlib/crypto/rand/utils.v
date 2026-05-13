// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

import math.bits

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
		// Inlined big-endian u64 read (avoids depending on `encoding.binary`).
		z[k] = u64(b[i - 8]) << 56 | u64(b[i - 7]) << 48 | u64(b[i - 6]) << 40 | u64(b[i - 5]) << 32 | u64(b[i - 4]) << 24 | u64(b[i - 3]) << 16 | u64(b[i - 2]) << 8 | u64(b[i - 1])
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
