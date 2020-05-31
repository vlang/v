// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

// TODO: Remove these functions once done:
// 1. C.rand()
// 2. seed()
// 3. next()
// 4. rand_r()
// fn C.rand() int
pub fn seed(s int) {
	C.srand(s)
}

pub fn next(max int) int {
	return C.rand() % max
}

// rand_r returns a pseudo-random number;
// writes a result value to the seed argument.
pub fn rand_r(seed &int) int {
	ns := *seed * 1103515245 + 12345
	unsafe {
		(*seed) = ns
	}
	return ns & 0x7fffffff
}

const (
	default_rng = new_default({})
)

pub struct PRNGConfigStruct {
	seed []u32 = time_seed_array(1)
}

pub fn new_default(config PRNGConfigStruct) &SysRNG {
	rng := &SysRNG{}
	rng.seed(config.seed)
	return rng
}


// u32() - returns a uniformly distributed pseudorandom 32-bit unsigned u32
pub fn u32() u32 {
	return default_rng.u32()
}

// u64() - returns a uniformly distributed pseudorandom 64-bit unsigned u64
pub fn u64() u64 {
	return default_rng.u64()
}

// u32n(max) - returns a uniformly distributed pseudorandom 32-bit unsigned u32 in [0, max)
pub fn u32n(max u32) u32 {
	return default_rng.u32n(max)
}

// u64n(max) - returns a uniformly distributed pseudorandom 64-bit unsigned u64 in [0, max)
pub fn u64n(max u64) u64 {
	return default_rng.u64n(max)
}

// u32n() - returns a uniformly distributed pseudorandom 32-bit unsigned u32 in [min, max)
pub fn u32_in_range(min, max u32) u32 {
	return default_rng.u32_in_range(min, max)
}

// u64_in_range(min, max) - returns a uniformly distributed pseudorandom 64-bit unsigned u64 in [min, max)
pub fn u64_in_range(min, max u64) u64 {
	return default_rng.u64_in_range(min, max)
}

// int() - returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) int
pub fn int() int {
	return default_rng.int()
}

// intn(max) - returns a uniformly distributed pseudorandom 32-bit signed positive int in [0, max)
pub fn intn(max int) int {
	return default_rng.intn(max)
}

// int_in_range(min, max) - returns a uniformly distributed pseudorandom
// 32-bit signed int in [min, max)
pub fn int_in_range(min, max int) int {
	return default_rng.int_in_range(min, max)
}

// int31() - returns a uniformly distributed pseudorandom 31-bit signed positive int
pub fn int31() int {
	return default_rng.int31()
}

// i64() - returns a uniformly distributed pseudorandom 64-bit signed (possibly negative) i64
pub fn i64() i64 {
	return default_rng.i64()
}

// i64n(max) - returns a uniformly distributed pseudorandom 64-bit signed positive i64 in [0, max)
pub fn i64n(max i64) i64 {
	return default_rng.i64n(max)
}

// i64_in_range(min, max) - returns a uniformly distributed pseudorandom
// 64-bit signed int in [min, max)
pub fn i64_in_range(min, max i64) i64 {
	return default_rng.i64_in_range(min, max)
}

// int63() - returns a uniformly distributed pseudorandom 63-bit signed positive int
pub fn int63() i64 {
	return default_rng.int63()
}

// f32() - returns a uniformly distributed 32-bit floating point in [0, 1)
pub fn f32() f32 {
	return default_rng.f32()
}

// f64() - returns a uniformly distributed 64-bit floating point in [0, 1)
pub fn f64() f64 {
	return default_rng.f64()
}

// f32n() - returns a uniformly distributed 32-bit floating point in [0, max)
pub fn f32n(max f32) f32 {
	return default_rng.f32n(max)
}

// f64n() - returns a uniformly distributed 64-bit floating point in [0, max)
pub fn f64n(max f64) f64 {
	return default_rng.f64n(max)
}

// f32_in_range(min, max) - returns a uniformly distributed 32-bit floating point in [min, max)
pub fn f32_in_range(min, max f32) f32 {
	return default_rng.f32_in_range(min, max)
}

// f64_in_range(min, max) - returns a uniformly distributed 64-bit floating point in [min, max)
pub fn f64_in_range(min, max f64) f64 {
	return default_rng.f64_in_range(min, max)
}

// rand_f32 return a random f32 between 0 and max
[deprecated]
pub fn rand_f32(max f32) f32 {
	return rand_uniform_f32() * max
}

// rand_f32 return a random f32 in range min and max
[deprecated]
pub fn rand_f32_in_range(min, max f32) f32 {
	return min + rand_uniform_f32() * (max - min)
}

// rand_f64 return a random f64 between 0 (inclusive) and max (exclusive)
[deprecated]
pub fn rand_f64(max f64) f64 {
	return rand_uniform_f64() * max
}

// rand_f64 return a random f64 in range min (inclusive) and max (exclusive)
[deprecated]
pub fn rand_f64_in_range(min, max f64) f64 {
	return min + rand_uniform_f64() * (max - min)
}

// rand_uniform_f32 returns a uniformly distributed f32 in the range 0 (inclusive) and 1 (exclusive)
[deprecated]
pub fn rand_uniform_f32() f32 {
	return f32(C.rand()) / f32(C.RAND_MAX)
}

// rand_uniform_f64 returns a uniformly distributed f64 in the range 0 (inclusive) and 1 (exclusive)
[deprecated]
pub fn rand_uniform_f64() f64 {
	return f64(C.rand()) / f64(C.RAND_MAX)
}
