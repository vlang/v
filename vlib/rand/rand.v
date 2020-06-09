// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import rand.util
import rand.wyrand

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