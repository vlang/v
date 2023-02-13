// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

// TODO: Remove num when no longer neded for debugging
fn new_worker(ch chan Workerfn, num int) thread {
	mut w := &Worker{
		ch: ch
		num: num
	}

	return spawn w.scan()
}

struct Worker {
	ch  chan Workerfn
	num int
}

pub fn (mut w Worker) scan() {
	for {
		func := <-w.ch or {
			println('Closing worker')
			return
		}

		func()
	}
}

type Workerfn = fn ()
