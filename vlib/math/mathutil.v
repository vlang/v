// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// min returns the minimum of `a` and `b`
[inline]
pub fn min<T>(a T, b T) T {
	return if a < b { a } else { b }
}

// max returns the maximum of `a` and `b`
[inline]
pub fn max<T>(a T, b T) T {
	return if a > b { a } else { b }
}

// abs returns the absolute value of `a`
[inline]
pub fn abs<T>(a T) T {
	return if a < 0 { -a } else { a }
}
