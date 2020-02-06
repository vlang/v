// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl

module math

// nextafter32 returns the next representable f32 value after x towards y.
//
// special cases are:
//	nextafter32(x, x)   = x
//	nextafter32(nan, y) = nan
//	nextafter32(x, nan) = nan
pub fn nextafter32(x, y f32) f32 {
        mut r := 0.0

	if is_nan(f64(x)) || is_nan(f64(y)) {
		r = f32(nan())
        }
	else if x == y {
		r = x
        }
	else if x == 0 {
		r = f32(copysign(f64(f32_from_bits(1)), f64(y)))
        }
	else if (y > x) == (x > 0) {
		r = f32_from_bits(f32_bits(x) + 1)
        }
	else {
		r = f32_from_bits(f32_bits(x) - 1)
        }

	return r
}

// nextafter returns the next representable f64 value after x towards y.
//
// special cases are:
//	nextafter(x, x)   = x
//	nextafter(nan, y) = nan
//	nextafter(x, nan) = nan
pub fn nextafter(x, y f64) f64 {
        mut r := f64(0)

	if is_nan(x) || is_nan(y) {
		r = nan()
        }
	else if x == y {
		r = x
        }
	else if x == 0 {
		r = copysign(f64_from_bits(1), y)
        }
	else if (y > x) == (x > 0) {
		r = f64_from_bits(f64_bits(x) + 1)
        }
	else {
		r = f64_from_bits(f64_bits(x) - 1)
        }

	return r
}
