// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sys

import math.bits
import rand.util

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
	u32_iter_count = calculate_iterations_for(32)
	u64_iter_count = calculate_iterations_for(64)
)

fn calculate_iterations_for(bits int) int {
	base := bits / rand_bitsize
	extra := if bits % rand_bitsize == 0 { 0 } else { 1 }
	return base + extra
}

// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
fn C.rand() int

// C.srand seeds the internal PRNG with the given int seed.
// fn C.srand(seed int)
// SysRNG is the PRNG provided by default in the libc implementiation that V uses.
pub struct SysRNG {
mut:
	seed u32 = util.time_seed_32()
}

// r.seed() sets the seed of the accepting SysRNG to the given data.
pub fn (mut r SysRNG) seed(seed_data []u32) {
	if seed_data.len != 1 {
		eprintln('SysRNG needs one 32-bit unsigned integer as the seed.')
		exit(1)
	}
	r.seed = seed_data[0]
	unsafe {C.srand(int(r.seed))}
}

// r.default_rand() exposes the default behavior of the system's RNG
// (equivalent to calling C.rand()). Recommended for testing/comparison
// b/w V and other languages using libc and not for regular use.
// This is also a one-off feature of SysRNG, similar to the global seed
// situation. Other generators will not have this.
[inline]
pub fn (r SysRNG) default_rand() int {
	return C.rand()
}

// r.u32() returns a pseudorandom u32 value less than 2^32
[inline]
pub fn (r SysRNG) u32() u32 {
	mut result := u32(C.rand())
	for i in 1 .. u32_iter_count {
		result = result ^ (u32(C.rand()) << (rand_bitsize * i))
	}
	return result
}

// r.u64() returns a pseudorandom u64 value less than 2^64
[inline]
pub fn (r SysRNG) u64() u64 {
	mut result := u64(C.rand())
	for i in 1 .. u64_iter_count {
		result = result ^ (u64(C.rand()) << (rand_bitsize * i))
	}
	return result
}

// r.u32n(max) returns a pseudorandom u32 value that is guaranteed to be less than max
[inline]
pub fn (r SysRNG) u32n(max u32) u32 {
	if max == 0 {
		eprintln('max must be positive integer')
		exit(1)
	}
	// Owing to the pigeon-hole principle, we can't simply do
	// val := rng.u32() % max.
	// It'll wreck the properties of the distribution unless
	// max evenly divides 2^32. So we divide evenly to
	// the closest power of two. Then we loop until we find
	// an int in the required range
	bit_len := bits.len_32(max)
	if bit_len == 32 {
		for {
			value := r.u32()
			if value < max {
				return value
			}
		}
	} else {
		mask := (u32(1) << (bit_len + 1)) - 1
		for {
			value := r.u32() & mask
			if value < max {
				return value
			}
		}
	}
	return u32(0)
}

// r.u64n(max) returns a pseudorandom u64 value that is guaranteed to be less than max
[inline]
pub fn (r SysRNG) u64n(max u64) u64 {
	if max == 0 {
		eprintln('max must be positive integer')
		exit(1)
	}
	// Similar procedure for u64s
	bit_len := bits.len_64(max)
	if bit_len == 64 {
		for {
			value := r.u64()
			if value < max {
				return value
			}
		}
	} else {
		mask := (u64(1) << (bit_len + 1)) - 1
		for {
			value := r.u64() & mask
			if value < max {
				return value
			}
		}
	}
	return u64(0)
}

// r.u32n(min, max) returns a pseudorandom u32 value that is guaranteed to be in [min, max)
[inline]
pub fn (r SysRNG) u32_in_range(min u32, max u32) u32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + r.u32n(max - min)
}

// r.u64n(min, max) returns a pseudorandom u64 value that is guaranteed to be in [min, max)
[inline]
pub fn (r SysRNG) u64_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + r.u64n(max - min)
}

// r.int() returns a pseudorandom 32-bit int (which may be negative)
[inline]
pub fn (r SysRNG) int() int {
	return int(r.u32())
}

// r.i64() returns a pseudorandom 64-bit i64 (which may be negative)
[inline]
pub fn (r SysRNG) i64() i64 {
	return i64(r.u64())
}

// r.int31() returns a pseudorandom 31-bit int which is non-negative
[inline]
pub fn (r SysRNG) int31() int {
	return int(r.u32() & util.u31_mask) // Set the 32nd bit to 0.
}

// r.int63() returns a pseudorandom 63-bit int which is non-negative
[inline]
pub fn (r SysRNG) int63() i64 {
	return i64(r.u64() & util.u63_mask) // Set the 64th bit to 0.
}

// r.intn(max) returns a pseudorandom int that lies in [0, max)
[inline]
pub fn (r SysRNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(r.u32n(u32(max)))
}

// r.i64n(max) returns a pseudorandom i64 that lies in [0, max)
[inline]
pub fn (r SysRNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(r.u64n(u64(max)))
}

// r.int_in_range(min, max) returns a pseudorandom int that lies in [min, max)
[inline]
pub fn (r SysRNG) int_in_range(min int, max int) int {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	// This supports negative ranges like [-10, -5) because the difference is positive
	return min + r.intn(max - min)
}

// r.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (r SysRNG) i64_in_range(min i64, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + r.i64n(max - min)
}

// r.f32() returns a pseudorandom f32 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (r SysRNG) f32() f32 {
	return f32(r.u32()) / util.max_u32_as_f32
}

// r.f64() returns a pseudorandom f64 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (r SysRNG) f64() f64 {
	return f64(r.u64()) / util.max_u64_as_f64
}

// r.f32n() returns a pseudorandom f32 value in [0, max)
[inline]
pub fn (r SysRNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return r.f32() * max
}

// r.f64n() returns a pseudorandom f64 value in [0, max)
[inline]
pub fn (r SysRNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return r.f64() * max
}

// r.f32_in_range(min, max) returns a pseudorandom f32 that lies in [min, max)
[inline]
pub fn (r SysRNG) f32_in_range(min f32, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + r.f32n(max - min)
}

// r.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (r SysRNG) f64_in_range(min f64, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + r.f64n(max - min)
}
