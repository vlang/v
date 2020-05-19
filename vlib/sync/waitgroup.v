// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

// [init_with=new_waitgroup] // TODO: implement support for init_with struct attribute, and disallow WaitGroup{} from outside the sync.new_waitgroup() function.
[ref_only]
pub struct WaitGroup {
mut:
	mu     &Mutex = &Mutex(0)
	active int
}

pub fn new_waitgroup() &WaitGroup {
	return &WaitGroup{mu: sync.new_mutex() }
}

pub fn (mut wg WaitGroup) add(delta int) {
	wg.mu.lock()
	wg.active += delta
	wg.mu.unlock()
	if wg.active < 0 {
		panic('Negative number of jobs in waitgroup')
	}
}

pub fn (mut wg WaitGroup) done() {
	wg.add(-1)
}

pub fn (wg &WaitGroup) wait() {
	for wg.active > 0 {
		// Do not remove this, busy empty loops are optimized
		// with -prod by some compilers, see issue #2874
		$if windows {
			C.Sleep(1)
		} $else {
			C.usleep(1000)
		}
	}
}
