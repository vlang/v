// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

// WaitGroup implementation. wait() blocks until all tasks complete.
// Do not copy an instance of WaitGroup, use a ref instead.
//
// Two mutexes are required so that wait() doesn't unblock on done()/add() before
// task_count becomes zero.
//
// [init_with=new_waitgroup] // TODO: implement support for init_with struct attribute, and disallow WaitGroup{} from outside the sync.new_waitgroup() function.
[heap]
struct WaitGroup {
mut:
	task_count       int // current task count
	task_count_mutex Mutex // This mutex protects the task_count count in add()
	wait_blocker     Semaphore // This blocks the wait() until released by add()
}

pub fn new_waitgroup() &WaitGroup {
	mut wg := &WaitGroup{}
	wg.task_count_mutex.init()
	wg.wait_blocker.init(1)
	return wg
}

// add increments (+ve delta) or decrements (-ve delta) task count by delta
// and unblocks any wait() calls if task count becomes zero.
// add panics if task count drops below zero.
pub fn (mut wg WaitGroup) add(delta int) {
	// protect task_count
	wg.task_count_mutex.@lock()
	defer {
		wg.task_count_mutex.unlock()
	}
	// If task_count likely to leave zero, set wait() to block
	if wg.task_count == 0 {
		wg.wait_blocker.wait()
	}
	wg.task_count += delta
	if wg.task_count < 0 {
		panic('Negative number of jobs in waitgroup')
	}
	if wg.task_count == 0 { // if no more task_count tasks
		wg.wait_blocker.post() // unblock wait()
	}
}

// done is a convenience fn for add(-1)
pub fn (mut wg WaitGroup) done() {
	wg.add(-1)
}

// wait blocks until all tasks are done (task count becomes zero)
pub fn (mut wg WaitGroup) wait() {
	wg.wait_blocker.wait() // blocks until task_count becomes 0
	wg.wait_blocker.post() // allow other wait()s to unblock or reuse wait group
}
