// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

// in ms
fn ticks() double {
	return double(0)
}

fn sleep(seconds int) {
	C.sleep(seconds)
}

fn sleep_ms(seconds int) {
	C.usleep(seconds * 1000)
}

