// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

import time

pub fn seed() {
	C.srand(time.now().uni)
}

pub fn next(max int) int {
	return C.rand() % max
}

fn C.rand() int 

