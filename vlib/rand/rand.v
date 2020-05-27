// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

const (
	max_i32 = 2147483647
)

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
	return f32(f64(C.rand()) / f64(C.RAND_MAX) * f64(max))
}

// rand_f32 return a random f32 in range min and max
pub fn rand_f32_in_range(min, max f32) f32 {
	scaled_r := f32(next(max_i32)) / max_i32
	return min + scaled_r * (max - min)
}

// rand_f64 return a random f64 between 0 and max
pub fn rand_f64(max f32) f32 {
	return f64(C.rand()) / f64(C.RAND_MAX) * f64(max)
}

// rand_f64 return a random f64 in range min and max
pub fn rand_f64_in_range(min, max f64) f64 {
	scaled_r := f64(next(max_i32)) / max_i32
	return min + scaled_r * (max - min)
}
