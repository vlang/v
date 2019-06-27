// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

// in ms
fn ticks() double {
	return C.GetTickCount()
}

fn sleep(seconds int) {
	C._sleep(seconds * 1000)
}

fn usleep(seconds int) {
	panic('usleep not impl')
	// C._usleep(seconds)
}

fn sleep_ms(n int) {
	C.Sleep(n)
}

