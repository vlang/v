// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

[trusted]
fn C.atomic_fetch_add_u32(voidptr, u32) u32

// WaitGroup
// Do not copy an instance of WaitGroup, use a ref instead.
//
// usage: in main thread:
// `wg := sync.new_waitgroup()
// `wg.add(nr_jobs)` before starting jobs with `go ...`
// `wg.wait()` to wait for all jobs to have finished
//
// in each parallel job:
// `wg.done()` when finished 
//
// [init_with=new_waitgroup] // TODO: implement support for init_with struct attribute, and disallow WaitGroup{} from outside the sync.new_waitgroup() function.
[heap]
struct WaitGroup {
mut:
	task_count u32 // current task count - reading/writing should be atomic
	sem        Semaphore // This blocks wait() until tast_countreleased by add()
}

pub fn new_waitgroup() &WaitGroup {
	mut wg := &WaitGroup{}
	wg.init()
	return wg
}

pub fn (mut wg WaitGroup) init() {
	wg.sem.init(0)
}

// add increments (+ve delta) or decrements (-ve delta) task count by delta
// and unblocks any wait() calls if task count becomes zero.
// add panics if task count drops below zero.
pub fn (mut wg WaitGroup) add(delta int) {
	old_nrjobs := int(C.atomic_fetch_add_u32(&wg.task_count, u32(delta)))
	new_nrjobs := old_nrjobs + delta
	if new_nrjobs < 0 {
		panic('Negative number of jobs in waitgroup')
	}
	if new_nrjobs == 0 {
		wg.sem.post()
	}
}

// done is a convenience fn for add(-1)
pub fn (mut wg WaitGroup) done() {
	wg.add(-1)
}

// wait blocks until all tasks are done (task count becomes zero)
pub fn (mut wg WaitGroup) wait() {
	wg.sem.wait() // blocks until task_count becomes 0
}
