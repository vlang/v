// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This is an implementation of wyhash v4
// from https://github.com/wangyi-fudan/wyhash
//
// Pure V implementation. It produces bit-identical output to the C `wyhash`
// and `wyhash64` helpers emitted by the C backend, so hashes stay stable
// across backends.
module hash

const wyp0 = u64(0x2d358dccaa6c78a5)
const wyp1 = u64(0x8bb84b93962eacc9)
const wyp2 = u64(0x4b33a62ed433d4a3)
const wyp3 = u64(0x4d5a2da51de1aa47)

// wymum returns a hash by performing multiply and mix on `a` and `b`.
@[ignore_overflow; inline]
pub fn wymum(a u64, b u64) u64 {
	/*
	mut r := u128(a)
	r = r*b
	return (r>>64)^r
	*/
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

// wyhash64_c returns a hash given two u64 values `a` and `b`.
pub fn wyhash64_c(a u64, b u64) u64 {
	return wymum(a ^ wyp0, u64(0xa0761d6478bd642f)) ^ wymum(b ^ wyp1, u64(0xe7037ed1a0b428db))
}

// wyhash_c returns a hash given a byte string `key`, its `len`, and a `seed`.
// The key is only read, never modified.
@[ignore_overflow]
pub fn wyhash_c(key &u8, len u64, seed u64) u64 {
	mut h := seed ^ wyp0 ^ (len * u64(0x9e3779b97f4a7c15))
	mut off := u64(0)
	mut n := len
	for n >= 8 {
		mut w := u64(0)
		unsafe {
			w = u64(key[off]) | (u64(key[off + 1]) << 8) | (u64(key[off + 2]) << 16) | (u64(key[off + 3]) << 24) | (u64(key[off + 4]) << 32) | (u64(key[off + 5]) << 40) | (u64(key[off + 6]) << 48) | (u64(key[off + 7]) << 56)
		}
		h = (h ^ w) * u64(0xa0761d6478bd642f)
		h ^= h >> 29
		off += 8
		n -= 8
	}
	if n > 0 {
		mut v := u64(0)
		unsafe {
			if n >= 7 {
				v |= u64(key[off + 6]) << 48
			}
			if n >= 6 {
				v |= u64(key[off + 5]) << 40
			}
			if n >= 5 {
				v |= u64(key[off + 4]) << 32
			}
			if n >= 4 {
				v |= u64(key[off + 3]) << 24
			}
			if n >= 3 {
				v |= u64(key[off + 2]) << 16
			}
			if n >= 2 {
				v |= u64(key[off + 1]) << 8
			}
			v |= u64(key[off])
		}
		h = (h ^ v) * u64(0xe7037ed1a0b428db)
		h ^= h >> 29
	}
	h *= u64(0x8ebc6af09c88c6e3)
	h ^= h >> 32
	h *= u64(0x589965cc75374cc3)
	h ^= h >> 29
	return h
}

// sum64_string returns a hash given a V string `key` and a `seed`.
@[inline]
pub fn sum64_string(key string, seed u64) u64 {
	return wyhash_c(key.str, u64(key.len), seed)
}

// sum64 returns a hash given a byte array `key` and a `seed`.
@[inline]
pub fn sum64(key []u8, seed u64) u64 {
	return wyhash_c(key.data, u64(key.len), seed)
}
