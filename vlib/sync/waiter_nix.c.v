// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

[ref_only]
struct Waiter{
mut:
	mx &Mutex
}

pub fn (mut w Waiter) wait() {
	w.mx.m_lock()
}

pub fn (mut w Waiter) stop() {
	w.mx.unlock()
}
pub fn new_waiter() &Waiter {
	w := &Waiter{mx: new_mutex()}
	return w
}
