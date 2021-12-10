// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module mathutil

[inline]
pub fn min<T>(a T, b T) T {
	return if a < b { a } else { b }
}

[inline]
pub fn max<T>(a T, b T) T {
	return if a > b { a } else { b }
}

[inline]
pub fn abs<T>(a T) T {
	return if a > 0 { a } else { -a }
}
