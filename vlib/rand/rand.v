// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module rand

import rand.config
import rand.wyrand

// PRNG is a common interface for all PRNGs that can be used seamlessly with the rand
// modules's API. It defines all the methods that a PRNG (in the vlib or custom made) must
// implement in order to ensure that _all_ functions can be used with the generator.
pub interface PRNG {
mut:
	seed(seed_data []u32)
	u32() u32
	u64() u64
	u32n(max u32) u32
	u64n(max u64) u64
	u32_in_range(min u32, max u32) u32
	u64_in_range(min u64, max u64) u64
	int() int
	i64() i64
	int31() int
	int63() i64
	intn(max int) int
	i64n(max i64) i64
	int_in_range(min int, max int) int
	i64_in_range(min i64, max i64) i64
	f32() f32
	f64() f64
	f32n(max f32) f32
	f64n(max f64) f64
	f32_in_range(min f32, max f32) f32
	f64_in_range(min f64, max f64) f64
	free()
}

__global default_rng &PRNG

// new_default returns a new instance of the default RNG. If the seed is not provided, the current time will be used to seed the instance.
[manualfree]
pub fn new_default(config config.PRNGConfigStruct) &PRNG {
	mut rng := &wyrand.WyRandRNG{}
	rng.seed(config.seed_)
	unsafe { config.seed_.free() }
	return &PRNG(rng)
}

// get_current_rng returns the PRNG instance currently in use. If it is not changed, it will be an instance of wyrand.WyRandRNG.
pub fn get_current_rng() &PRNG {
	return default_rng
}

// set_rng changes the default RNG from wyrand.WyRandRNG (or whatever the last RNG was) to the one
// provided by the user. Note that this new RNG must be seeded manually with a constant seed or the
// `seed.time_seed_array()` method. Also, it is recommended to store the old RNG in a variable and
// should be restored if work with the custom RNG is complete. It is not necessary to restore if the
// program terminates soon afterwards.
pub fn set_rng(rng &PRNG) {
	default_rng = unsafe { rng }
}

// seed sets the given array of `u32` values as the seed for the `default_rng`. The default_rng is
// an instance of WyRandRNG which takes 2 u32 values. When using a custom RNG, make sure to use
// the correct number of u32s.
pub fn seed(seed []u32) {
	default_rng.seed(seed)
}

// u32 returns a uniformly distributed `u32` in range `[0, 2³²)`.
pub fn u32() u32 {
	return default_rng.u32()
}

// u64 returns a uniformly distributed `u64` in range `[0, 2⁶⁴)`.
pub fn u64() u64 {
	return default_rng.u64()
}

// u32n returns a uniformly distributed pseudorandom 32-bit signed positive `u32` in range `[0, max)`.
pub fn u32n(max u32) u32 {
	return default_rng.u32n(max)
}

// u64n returns a uniformly distributed pseudorandom 64-bit signed positive `u64` in range `[0, max)`.
pub fn u64n(max u64) u64 {
	return default_rng.u64n(max)
}

// u32_in_range returns a uniformly distributed pseudorandom 32-bit unsigned `u32` in range `[min, max)`.
pub fn u32_in_range(min u32, max u32) u32 {
	return default_rng.u32_in_range(min, max)
}

// u64_in_range returns a uniformly distributed pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
pub fn u64_in_range(min u64, max u64) u64 {
	return default_rng.u64_in_range(min, max)
}

// int returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) `int`.
pub fn int() int {
	return default_rng.int()
}

// intn returns a uniformly distributed pseudorandom 32-bit signed positive `int` in range `[0, max)`.
pub fn intn(max int) int {
	return default_rng.intn(max)
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
pub fn byte() byte {
	return byte(default_rng.u32() & 0xff)
}

// int_in_range returns a uniformly distributed pseudorandom  32-bit signed int in range `[min, max)`.
// Both `min` and `max` can be negative, but we must have `min < max`.
pub fn int_in_range(min int, max int) int {
	return default_rng.int_in_range(min, max)
}

// int31 returns a uniformly distributed pseudorandom 31-bit signed positive `int`.
pub fn int31() int {
	return default_rng.int31()
}

// i64 returns a uniformly distributed pseudorandom 64-bit signed (possibly negative) `i64`.
pub fn i64() i64 {
	return default_rng.i64()
}

// i64n returns a uniformly distributed pseudorandom 64-bit signed positive `i64` in range `[0, max)`.
pub fn i64n(max i64) i64 {
	return default_rng.i64n(max)
}

// i64_in_range returns a uniformly distributed pseudorandom 64-bit signed `i64` in range `[min, max)`.
pub fn i64_in_range(min i64, max i64) i64 {
	return default_rng.i64_in_range(min, max)
}

// int63 returns a uniformly distributed pseudorandom 63-bit signed positive `i64`.
pub fn int63() i64 {
	return default_rng.int63()
}

// f32 returns a uniformly distributed 32-bit floating point in range `[0, 1)`.
pub fn f32() f32 {
	return default_rng.f32()
}

// f64 returns a uniformly distributed 64-bit floating point in range `[0, 1)`.
pub fn f64() f64 {
	return default_rng.f64()
}

// f32n returns a uniformly distributed 32-bit floating point in range `[0, max)`.
pub fn f32n(max f32) f32 {
	return default_rng.f32n(max)
}

// f64n returns a uniformly distributed 64-bit floating point in range `[0, max)`.
pub fn f64n(max f64) f64 {
	return default_rng.f64n(max)
}

// f32_in_range returns a uniformly distributed 32-bit floating point in range `[min, max)`.
pub fn f32_in_range(min f32, max f32) f32 {
	return default_rng.f32_in_range(min, max)
}

// f64_in_range returns a uniformly distributed 64-bit floating point in range `[min, max)`.
pub fn f64_in_range(min f64, max f64) f64 {
	return default_rng.f64_in_range(min, max)
}

const (
	english_letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
	hex_chars       = 'abcdef0123456789'
	ascii_chars     = '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{|}~'
)
