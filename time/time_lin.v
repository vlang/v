// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

// in ms
pub fn ticks() f64 {
	return f64(0)
}

pub fn sleep(seconds int) {
	C.sleep(seconds)
}

pub fn sleep_ms(seconds int) {
	C.usleep(seconds * 1000)
}

