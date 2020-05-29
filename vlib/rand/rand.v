// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

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

// rand_f32 return a random f32 between 0 and max
pub fn rand_f32(max f32) f32 {
	return rand_uniform_f32() * max
}

// rand_f32 return a random f32 in range min and max
pub fn rand_f32_in_range(min, max f32) f32 {
	return min + rand_uniform_f32() * (max - min)
}

// rand_f64 return a random f64 between 0 (inclusive) and max (exclusive)
pub fn rand_f64(max f64) f64 {
	return rand_uniform_f64() * max
}

// rand_f64 return a random f64 in range min (inclusive) and max (exclusive)
pub fn rand_f64_in_range(min, max f64) f64 {
	return min + rand_uniform_f64() * (max - min)
}

// rand_uniform_f32 returns a uniformly distributed f32 in the range 0 (inclusive) and 1 (exclusive)
pub fn rand_uniform_f32() f32 {
	return f32(C.rand()) / f32(C.RAND_MAX)
}

// rand_uniform_f64 returns a uniformly distributed f64 in the range 0 (inclusive) and 1 (exclusive)
pub fn rand_uniform_f64() f64 {
	return f64(C.rand()) / f64(C.RAND_MAX)
}