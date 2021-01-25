// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

[ref_only]
struct Waiter{
mut:
	sem C.sem_t
}

pub fn (mut w Waiter) wait() {
	unsafe { C.sem_wait(&w.sem) }
}

pub fn (mut w Waiter) stop() {
	unsafe { C.sem_post(&w.sem) }
}
pub fn new_waiter() &Waiter {
	w := &Waiter{}
	unsafe { C.sem_init(&w.sem, 0, 1) }
	return w
}
