// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// min returns the minimum of `a` and `b`.
// For floating point types, if either argument is NaN the result is NaN,
// matching Go's `min` builtin, which returns NaN regardless of argument order.
// See https://github.com/vlang/v/issues/29833 .
@[inline]
pub fn min[T](a T, b T) T {
	$if T is f32 || T is f64 {
		// A comparison against NaN is false, so without this guard the plain
		// `if a < b` below would return `b` unchanged and the result would
		// depend on which argument held the NaN.
		if a != a || b != b {
			return T(math.nan())
		}
		if b < a {
			return b
		}
		return a
	} $else {
		if b < a {
			return b
		}
		return a
	}
}

// max returns the maximum of `a` and `b`.
// For floating point types, if either argument is NaN the result is NaN,
// matching Go's `max` builtin, which returns NaN regardless of argument order.
// See https://github.com/vlang/v/issues/29833 .
@[inline]
pub fn max[T](a T, b T) T {
	$if T is f32 || T is f64 {
		if a != a || b != b {
			return T(math.nan())
		}
		if b > a {
			return b
		}
		return a
	} $else {
		if b > a {
			return b
		}
		return a
	}
}

// abs returns the absolute value of `a`. Floating point zero always becomes positive zero.
@[inline]
pub fn abs[T](a T) T {
	$if T is $float {
		if a == 0 {
			return T(0)
		}
	}
	return if a < 0 { -a } else { a }
}
