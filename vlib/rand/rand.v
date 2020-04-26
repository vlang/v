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
	(*seed) = ns
	return ns & 0x7fffffff
}
