// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This is an implementation of wyhash v4
// from https://github.com/wangyi-fudan/wyhash
//
// Pure V implementation replacing the former C backend.
module hash

const wyp0 = u64(0x2d358dccaa6c78a5)
const wyp1 = u64(0x8bb84b93962eacc9)
const wyp2 = u64(0x4b33a62ed433d4a3)
const wyp3 = u64(0x4d5a2da51de1aa47)

// wymum returns a hash by performing multiply and mix on `a` and `b`.
// It simulates a 128-bit multiplication and XORs the high and low parts.
@[ignore_overflow; inline]
pub fn wymum(a u64, b u64) u64 {
	mask32 := u32(4294967295)
	x0 := a & mask32
	x1 := a >> 32
	y0 := b & mask32
	y1 := b >> 32
	w0 := x0 * y0
	t := x1 * y0 + (w0 >> 32)
	mut w1 := t & mask32
	w2 := t >> 32
	w1 += x0 * y1
	hi := x1 * y1 + w2 + (w1 >> 32)
	lo := a * b
	return hi ^ lo
}

// wymix performs a mixing operation on two values using wymum.
@[inline]
fn wymix(a u64, b u64) u64 {
	return wymum(a, b) ^ b
}

// wyhash64_c returns a 64-bit hash of two u64 values `a` and `b`.
// This is useful for hashing map keys that are 128-bit composite.
pub fn wyhash64_c(a u64, b u64) u64 {
	x := a ^ wyp0
	y := b ^ wyp1
	return wymix(x, y)
}

// wyhash_c returns a hash of a byte sequence given a key pointer, length, and seed.
@[inline]
pub fn wyhash_c(key &u8, len u64, seed u64) u64 {
	mut z := key // mutable pointer for traversal
	mut n := len
	mut result := seed ^ wyp1

	// Process 32-byte chunks
	for n >= 32 {
		// Read 4 x u64 from the chunk
		a0 := unsafe { *&u64(z) }
		a1 := unsafe { *&u64(z + 8) }
		a2 := unsafe { *&u64(z + 16) }
		a3 := unsafe { *&u64(z + 24) }

		// Mix and store
		unsafe { *&u64(z + 8) = a0 ^ wyp0 }
		unsafe { *&u64(z + 16) = a1 ^ seed ^ wyp2 }
		unsafe { *&u64(z + 24) = a2 ^ a3 ^ wyp3 }

		z = unsafe { z + 32 }
		n -= 32

		// Update result with seed mixing
		result = wymix(result ^ a0 ^ a1, result ^ a2 ^ a3)
	}

	// Process remaining bytes (short path)
	mut tmp := u64(0)
	if n >= 4 {
		// Read u32 from start and u32 from end
		s0 := unsafe { *&u32(z) }
		s1 := unsafe { *&u32(z + n - 4) }
		tmp = (u64(s0) << 16) | (s1 >> 16)
	} else if n >= 2 {
		s0 := unsafe { *&u16(z) }
		s1 := unsafe { *&u16(z + n - 2) }
		tmp = u64(s0) | (u64(s1) << 8)
	} else if n == 1 {
		tmp = u64(unsafe { *z })
	}

	if n > 0 {
		result = wymix(result, tmp ^ seed ^ len)
	}

	return result
}

// sum64_string returns a hash of a V string `key` with a `seed`.
pub fn sum64_string(key string, seed u64) u64 {
	return wyhash_c(key.str, u64(key.len), seed)
}

// sum64 returns a hash of a byte array `key` with a `seed`.
pub fn sum64(key []u8, seed u64) u64 {
	if key.len == 0 {
		return wyhash64_c(0, seed)
	}
	return wyhash_c(key.data, u64(key.len), seed)
}
