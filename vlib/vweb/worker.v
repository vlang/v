// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

fn new_worker(ch chan Workerfn) thread {
	mut w := &Worker{
		ch: ch
	}

	return spawn w.scan()
}

struct Worker {
	ch chan Workerfn
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
