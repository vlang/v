// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// min returns the minimum of `a` and `b`
@[inline]
pub fn min[T](a T, b T) T {
	return if a < b { a } else { b }
}

// max returns the maximum of `a` and `b`
@[inline]
pub fn max[T](a T, b T) T {
	return if a > b { a } else { b }
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
