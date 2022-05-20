// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sys

import math.bits
import rand.buffer
import rand.seed

// Implementation note:
// ====================
// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
// C.rand() is okay to use within its defined range.
// (See: https://web.archive.org/web/20180801210127/http://eternallyconfuzzled.com/arts/jsw_art_rand.aspx)
// The problem is, this value varies with the libc implementation. On windows,
// for example, RAND_MAX is usually a measly 32767, whereas on (newer) linux it's generally
// 2147483647. The repetition period also varies wildly. In order to provide more entropy
// without altering the underlying algorithm too much, this implementation simply
// requests for more random bits until the necessary width for the integers is achieved.

pub const seed_len = 1

const (
	rand_limit     = u64(C.RAND_MAX)
	rand_bitsize   = bits.len_64(rand_limit)
	rand_bytesize  = rand_bitsize / 8
	u16_iter_count = calculate_iterations_for(16)
	u32_iter_count = calculate_iterations_for(32)
	u64_iter_count = calculate_iterations_for(64)
)

fn calculate_iterations_for(bits int) int {
	base := bits / sys.rand_bitsize
	extra := if bits % sys.rand_bitsize == 0 { 0 } else { 1 }
	return base + extra
}

// SysRNG is the PRNG provided by default in the libc implementiation that V uses.
pub struct SysRNG {
	buffer.PRNGBuffer
mut:
	seed u32 = seed.time_seed_32()
}

// r.seed() sets the seed of the accepting SysRNG to the given data.
pub fn (mut r SysRNG) seed(seed_data []u32) {
	if seed_data.len != 1 {
		eprintln('SysRNG needs one 32-bit unsigned integer as the seed.')
		exit(1)
	}
	r.seed = seed_data[0]
	C.srand(r.seed)
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

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
pub fn (mut r SysRNG) u8() u8 {
	if r.bytes_left >= 1 {
		r.bytes_left -= 1
		value := u8(r.buffer)
		r.buffer >>= 8
		return value
	}
	r.buffer = u64(r.default_rand())
	r.bytes_left = sys.rand_bytesize - 1
	value := u8(r.buffer)
	r.buffer >>= 8
	return value
}

// u16 returns a uniformly distributed pseudorandom 16-bit unsigned positive `u16`.
[inline]
pub fn (mut r SysRNG) u16() u16 {
	if r.bytes_left >= 2 {
		r.bytes_left -= 2
		value := u16(r.buffer)
		r.buffer >>= 16
		return value
	}
	mut result := u16(C.rand())
	for i in 1 .. sys.u16_iter_count {
		result = result ^ (u16(C.rand()) << (sys.rand_bitsize * i))
	}
	return result
}

// u32 returns a uniformly distributed pseudorandom 32-bit unsigned positive `u32`.
[inline]
pub fn (r SysRNG) u32() u32 {
	mut result := u32(C.rand())
	for i in 1 .. sys.u32_iter_count {
		result = result ^ (u32(C.rand()) << (sys.rand_bitsize * i))
	}
	return result
}

// u64 returns a uniformly distributed pseudorandom 64-bit unsigned positive `u64`.
[inline]
pub fn (r SysRNG) u64() u64 {
	mut result := u64(C.rand())
	for i in 1 .. sys.u64_iter_count {
		result = result ^ (u64(C.rand()) << (sys.rand_bitsize * i))
	}
	return result
}

// block_size returns the number of bits that the RNG can produce in a single iteration.
[inline]
pub fn (r SysRNG) block_size() int {
	return sys.rand_bitsize
}

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng SysRNG) free() {
	unsafe { free(rng) }
}
