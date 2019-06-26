// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

//#include <time.h>


struct LOLLO {
}

fn kek() {
}

pub fn seed() {
	# time_t t;
	# srand((unsigned) time(&t));
}

fn ffkek() {
}

pub fn next(max int) int {
	r := 0 
	# r = rand();  // TODO parser bug `rand` module name conflict 
	return r % max 
}

struct C.time_t{} 
fn C.rand() int 

