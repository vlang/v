// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import math.bits

// Implementation note:
// ====================
// C.rand() is okay to use within its defined range of C.RAND_MAX.
// (See: https://web.archive.org/web/20180801210127/http://eternallyconfuzzled.com/arts/jsw_art_rand.aspx)
// The problem is, this value varies with the libc implementation. On windows,
// for example, RAND_MAX is usually a measly 32767, whereas on (newer) linux it's generaly
// 2147483647. The repetition period also varies wildly. In order to provide more entropy
// without altering the underlying algorithm too much, this implementation simply
// requests for more random bits until the necessary width for the integers is achieved.
const (
	rand_limit     = u64(C.RAND_MAX)
	rand_bitsize   = bits.len_64(rand_limit)
	u32_iter_count = (32 / rand_bitsize) + 1
	u64_iter_count = (64 / rand_bitsize) + 1
)

// Size constants to avoid importing the entire math module
const (
	max_u32        = 4294967295
	max_u64        = 18446744073709551615
	max_u32_as_f32 = f32(max_u32)
	max_u64_as_f64 = f64(max_u64)
)

// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
fn C.rand() int

// C.srand seeds the internal PRNG with the given int seed.
fn C.srand(seed int)

// SysRNG is the PRNG provided by default in the libc implementiation that V uses.
pub struct SysRNG {
mut:
	seed u32 = u32(0x1008)
}

// r.seed() sets the seed of the accepting SysRNG to the given data.
pub fn (mut r SysRNG) seed(seed_data []u32) {
	if seed_data.len != 1 {
		panic('SysRNG needs one 32-bit unsigned integer as the seed.')
	}
	r.seed = seed_data[0]
	C.srand(int(r.seed))
}

// r.u32() returns a pseudorandom u32 value less than 2^32
pub fn (r SysRNG) u32() u32 {
	mut result := u32(C.rand())
	for _ in 0 .. u32_iter_count {
		result = (result << rand_bitsize) ^ u32(C.rand())
	}
	return result
}

// r.u64() returns a pseudorandom u64 value less than 2^64
pub fn (r SysRNG) u64() u64 {
	mut result := u64(C.rand())
	for _ in 0 .. u64_iter_count {
		result = (result << rand_bitsize) ^ u64(C.rand())
	}
	return result
}

pub fn (r SysRNG) u32n(max u32) u32 {
	// Owing to the pigeon-hole principle, we can't simply do
	// val := rng.u32() % max.
	// It'll wreck the properties of the distribution unless
	// max evenly divides 2^32. So we divide evenly to
	// the closest power of two. Then we loop until we find
	// an int int the required range
	bit_len := bits.len_32(max)
	if bit_len == 32 {
		return r.u32()
	}
	div := u32(1) << (bit_len + 1)
	for {
		value := r.u32() % div
		if value < max {
			return value
		}
	}
	return u32(0)
}

pub fn (r SysRNG) u64n(max u64) u64 {
	// Similar procedure for u64s
	bit_len := bits.len_64(max)
	if bit_len == 64 {
		return r.u64()
	}
	div := u64(1) << (bit_len + 1)
	for {
		value := r.u64() % div
		if value < max {
			return value
		}
	}
	return u64(0)
}

pub fn (r SysRNG) u32_in_range(min, max u32) u32 {
	if max <= min {
		panic('max must be greater than min')
	}
	return min + r.u32n(max - min)
}

pub fn (r SysRNG) u64_in_range(min, max u64) u64 {
	if max <= min {
		panic('max must be greater than min')
	}
	return min + r.u64n(max - min)
}

pub fn (r SysRNG) int() int {
	return int(r.u32())
}

pub fn (r SysRNG) intn(max int) int {
	if max <= 0 {
		panic('max has to be positive.')
	}
	return int(r.u32n(max))
}

pub fn (r SysRNG) int_in_range(min, max int) {
	if max <= min {
		panic('max must be greater than min')
	}
	return min + r.intn(max - min)
}

// r.f32() returns a pseudorandom f32 value between 0.0 (inclusive) and 1.0 (exclusive)
pub fn (r SysRNG) f32() f32 {
	return f32(r.u32()) / max_u32_as_f32
}

// r.f64() returns a pseudorandom f64 value between 0.0 (inclusive) and 1.0 (exclusive)
pub fn (r SysRNG) f64() f64 {
	return f64(r.u64()) / max_u64_as_f64
}
