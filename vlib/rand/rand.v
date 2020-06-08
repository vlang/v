// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import rand.util
import rand.wyrand

// TODO: Remove these functions once done:
// 1. C.rand()
// 2. seed()
// 3. next()
// 4. rand_r()
fn C.rand() int
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

pub struct PRNGConfigStruct {
	seed []u32 = util.time_seed_array(2)
}

const (
	default_rng = new_default({})
)

pub fn new_default(config PRNGConfigStruct) &wyrand.WyRandRNG {
	rng := &wyrand.WyRandRNG{}
	rng.seed(config.seed)
	return rng
}

// u32() - returns a uniformly distributed u32 in [0, 2**32)
pub fn u32() u32 {
	unsafe {
		mut x := default_rng
		return x.u32()
	}
	return 0
}

// u64() - returns a uniformly distributed u64 in [0, 2**64)
pub fn u64() u64 {
	unsafe {
		mut x := default_rng
		return x.u64()
	}
	return 0
}

// u32n(max) - returns a uniformly distributed pseudorandom 32-bit signed positive u32 in [0, max)
pub fn u32n(max u32) u32 {
	unsafe {
		mut x := default_rng
		return x.u32n(max)
	}
	return 0
}

// u64n(max) - returns a uniformly distributed pseudorandom 64-bit signed positive u64 in [0, max)
pub fn u64n(max u64) u64 {
	unsafe {
		mut x := default_rng
		return x.u64n(max)
	}
	return 0
}

// u32_in_range(min, max) - returns a uniformly distributed pseudorandom 32-bit unsigned u32 in [min, max)
pub fn u32_in_range(min, max u32) u32 {
	unsafe {
		mut x := default_rng
		return x.u32_in_range(min, max)
	}
	return 0
}

// u64_in_range(min, max) - returns a uniformly distributed pseudorandom 64-bit unsigned u64 in [min, max)
pub fn u64_in_range(min, max u64) u64 {
	unsafe {
		mut x := default_rng
		return x.u64_in_range(min, max)
	}
	return 0
}

// int() - returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) int
pub fn int() int {
	unsafe {
		mut x := default_rng
		return x.int()
	}
	return 0
}

// intn(max) - returns a uniformly distributed pseudorandom 32-bit signed positive int in [0, max)
pub fn intn(max int) int {
	unsafe {
		mut x := default_rng
		return x.intn(max)
	}
	return 0
}

// int_in_range(min, max) - returns a uniformly distributed pseudorandom
// 32-bit signed int in [min, max). Both min and max can be negative, but we must have `min < max`.
pub fn int_in_range(min, max int) int {
	unsafe {
		mut x := default_rng
		return x.int_in_range(min, max)
	}
	return 0
}

// int31() - returns a uniformly distributed pseudorandom 31-bit signed positive int
pub fn int31() int {
	unsafe {
		mut x := default_rng
		return x.int31()
	}
	return 0
}

// i64() - returns a uniformly distributed pseudorandom 64-bit signed (possibly negative) i64
pub fn i64() i64 {
	unsafe {
		mut x := default_rng
		return x.i64()
	}
	return 0
}

// i64n(max) - returns a uniformly distributed pseudorandom 64-bit signed positive i64 in [0, max)
pub fn i64n(max i64) i64 {
	unsafe {
		mut x := default_rng
		return x.i64n(max)
	}
	return 0
}

// i64_in_range(min, max) - returns a uniformly distributed pseudorandom
// 64-bit signed int in [min, max)
pub fn i64_in_range(min, max i64) i64 {
	unsafe {
		mut x := default_rng
		return x.i64_in_range(min, max)
	}
	return 0
}

// int63() - returns a uniformly distributed pseudorandom 63-bit signed positive int
pub fn int63() i64 {
	unsafe {
		mut x := default_rng
		return x.int63()
	}
	return 0
}

// f32() - returns a uniformly distributed 32-bit floating point in [0, 1)
pub fn f32() f32 {
	unsafe {
		mut x := default_rng
		return x.f32()
	}
	return 0
}

// f64() - returns a uniformly distributed 64-bit floating point in [0, 1)
pub fn f64() f64 {
	unsafe {
		mut x := default_rng
		return x.f64()
	}
	return 0
}

// f32n() - returns a uniformly distributed 32-bit floating point in [0, max)
pub fn f32n(max f32) f32 {
	unsafe {
		mut x := default_rng
		return x.f32n(max)
	}
	return 0
}

// f64n() - returns a uniformly distributed 64-bit floating point in [0, max)
pub fn f64n(max f64) f64 {
	unsafe {
		mut x := default_rng
		return x.f64n(max)
	}
	return 0
}

// f32_in_range(min, max) - returns a uniformly distributed 32-bit floating point in [min, max)
pub fn f32_in_range(min, max f32) f32 {
	unsafe {
		mut x := default_rng
		return x.f32_in_range(min, max)
	}
	return 0
}

// f64_in_range(min, max) - returns a uniformly distributed 64-bit floating point in [min, max)
pub fn f64_in_range(min, max f64) f64 {
	unsafe {
		mut x := default_rng
		return x.f64_in_range(min, max)
	}
	return 0
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
