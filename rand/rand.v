// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <time.h>
// #include <stdlib.h>
fn seed() {
	# time_t t;
	# srand((unsigned) time(&t));
}

fn next(max int) int {
	# return  rand() % max;
	return 4
}

