// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

pub fn seed(s int) {
	C.srand(s)
}

pub fn next(max int) int {
	return C.rand() % max
}

fn C.rand() int

/**
 * rand_r - reentrant pseudo-random number generator
 *
 * @param seed byref reentrant seed, holding current state
 *
 * @return a value between 0 and C.RAND_MAX (inclusive)
 */
pub fn rand_r(seed &int) int {
	mut rs := seed
	ns := ( *rs * 1103515245 + 12345 )
	*rs = ns
	return ns & 0x7fffffff
}
