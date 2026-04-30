// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

@[trusted]
fn C.atomic_load_u64(voidptr) u64

@[trusted]
fn C.atomic_store_u64(voidptr, u64)

@[trusted]
fn C.atomic_compare_exchange_weak_u64(voidptr, voidptr, u64) bool

@[trusted]
fn C.atomic_fetch_add_u64(voidptr, u64) u64

// WaitGroup
// Do not copy an instance of WaitGroup, use a ref instead.
//
// usage: in main thread:
// `wg := sync.new_waitgroup()`
// `wg.add(nr_jobs)` before starting jobs with `go ...`
// `wg.wait()` to wait for all jobs to have finished
//
// in each parallel job:
// `wg.done()` when finished
//
// [init_with=new_waitgroup] // TODO: implement support for init_with struct attribute, and disallow WaitGroup{} from outside the sync.new_waitgroup() function.
@[heap]
pub struct WaitGroup {
mut:
	state u64       // high 32 bits: task count, low 32 bits: wait count
	sem   Semaphore // Blocks wait() until add() drops the task count to zero
}

// new_waitgroup creates a new WaitGroup.
pub fn new_waitgroup() &WaitGroup {
	mut wg := WaitGroup{}
	wg.init()
	return &wg
}

// init initializes a WaitGroup.
pub fn (mut wg WaitGroup) init() {
	C.atomic_store_u64(voidptr(&wg.state), 0)
	wg.sem.init(0)
}

// add increments (+ve delta) or decrements (-ve delta) task count by delta
// and unblocks any wait() calls if task count becomes zero.
// add panics if task count drops below zero.
pub fn (mut wg WaitGroup) add(delta int) {
	state_delta := u64(u32(delta)) << 32
	old_state := C.atomic_fetch_add_u64(voidptr(&wg.state), state_delta)
	new_state := old_state + state_delta
	new_nrjobs := int(i32(new_state >> 32))
	mut num_waiters := u32(new_state)
	if new_nrjobs < 0 {
		panic('Negative number of jobs in waitgroup')
	}
	if new_nrjobs > 0 || num_waiters == 0 {
		return
	}
	if C.atomic_load_u64(voidptr(&wg.state)) != new_state {
		panic('WaitGroup misuse: add() called concurrently with wait()')
	}
	C.atomic_store_u64(voidptr(&wg.state), 0)
	for num_waiters > 0 {
		wg.sem.post()
		num_waiters--
	}
}

// done is a convenience fn for add(-1).
pub fn (mut wg WaitGroup) done() {
	wg.add(-1)
}

// wait blocks until all tasks are done (task count becomes zero).
pub fn (mut wg WaitGroup) wait() {
	for {
		mut state := C.atomic_load_u64(voidptr(&wg.state))
		nrjobs := u32(state >> 32)
		if nrjobs == 0 {
			return
		}
		if C.atomic_compare_exchange_weak_u64(voidptr(&wg.state), voidptr(&state), state + 1) {
			wg.sem.wait() // blocks until task_count becomes 0
			if C.atomic_load_u64(voidptr(&wg.state)) != 0 {
				panic('WaitGroup misuse: reused before previous wait() returned')
			}
			return
		}
	}
}

// go starts `f` in a new thread, arranging to call wg.add(1) before that,
// and wg.done() in the same thread. The function `f` should not panic.
// Calls to wg.go() should happen before the call to wg.wait().
pub fn (mut wg WaitGroup) go(f fn ()) {
	wg.add(1)
	spawn fn (mut wg WaitGroup, f fn ()) {
		f()
		wg.done()
	}(mut wg, f)
}
