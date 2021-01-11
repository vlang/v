module strconv

/*

f32/f64 ftoa functions

Copyright (c) 2019-2021 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the f32/f64 ftoa functions

These functions are based on the work of:
Publication:PLDI 2018: Proceedings of the 39th ACM SIGPLAN
Conference on Programming Language Design and ImplementationJune 2018
Pages 270–282 https://doi.org/10.1145/3192366.3192369

inspired by the Go version here:
https://github.com/cespare/ryu/tree/ba56a33f39e3bbbfa409095d0f9ae168a595feea

*/

[inline]
pub fn ftoa_64(f f64) string {
	return f64_to_str(f,17)
}

[inline]
pub fn ftoa_long_64(f f64) string {
	return f64_to_str_l(f)
}

[inline]
pub fn ftoa_32(f f32) string {
	return f32_to_str(f,8)
}

[inline]
pub fn ftoa_long_32(f f32) string {
	return f32_to_str_l(f)
}
