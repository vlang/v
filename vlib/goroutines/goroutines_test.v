// vtest build: !true // goroutines module is experimental; skip on all CIs for now
// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module goroutines

import time

// The Boehm GC cannot scan goroutine stacks yet: a collection while a goroutine runs
// crashes in GC_mark_from. Keep collections off while these tests run goroutines.
fn testsuite_begin() {
	gc_disable()
}

fn testsuite_end() {
	gc_enable()
}

// Helper function for test_goroutine_create (must be a plain function, not a closure).
fn goroutine_test_worker(arg voidptr) {
	// Signal that the goroutine ran by writing to the shared flag
	if arg != unsafe { nil } {
		unsafe {
			mut flag := &bool(arg)
			*flag = true
		}
	}
}

// Test basic goroutine creation
fn test_goroutine_create() {
	mut done := false
	goroutine_create(voidptr(goroutine_test_worker), voidptr(&done), 0)
	// Give the goroutine time to run
	time.sleep(100 * time.millisecond)
}

// Test channel make
fn test_chan_make() {
	c := chan_make(int(sizeof(int)), 10)
	assert c != unsafe { nil }
	assert chan_cap(c) == 10
	assert chan_len(c) == 0
}

// Test buffered channel send/recv
fn test_chan_buffered() {
	c := chan_make(int(sizeof(int)), 5)
	mut val := 42
	assert chan_send(c, voidptr(&val), false) == true
	assert chan_len(c) == 1

	mut recv_val := 0
	received, ok := chan_recv(c, voidptr(&recv_val), false)
	assert received == true
	assert ok == true
	assert recv_val == 42
	assert chan_len(c) == 0
}

// Test channel close
fn test_chan_close() {
	c := chan_make(int(sizeof(int)), 1)
	mut val := 100
	chan_send(c, voidptr(&val), false)
	chan_close(c)

	// Should still receive buffered value
	mut recv_val := 0
	received, ok := chan_recv(c, voidptr(&recv_val), false)
	assert received == true
	// After close with data, ok should be true
}

// Test GoroutineQueue operations
fn test_gqueue() {
	mut q := GoroutineQueue{}
	assert q.empty()
	assert q.size == 0

	mut g1 := &Goroutine{
		id: 1
	}
	mut g2 := &Goroutine{
		id: 2
	}
	mut g3 := &Goroutine{
		id: 3
	}

	q.push_back(g1)
	assert !q.empty()
	assert q.size == 1

	q.push_back(g2)
	q.push_back(g3)
	assert q.size == 3

	gp := q.pop()
	assert gp.id == 1
	assert q.size == 2

	gp2 := q.pop()
	assert gp2.id == 2

	gp3 := q.pop()
	assert gp3.id == 3
	assert q.empty()
}

// Test GoroutineList operations
fn test_glist() {
	mut l := GoroutineList{}
	assert l.empty()

	mut g1 := &Goroutine{
		id: 10
	}
	mut g2 := &Goroutine{
		id: 20
	}

	l.push(g1)
	assert !l.empty()
	assert l.count == 1

	l.push(g2)
	assert l.count == 2

	// GoroutineList is a stack (LIFO)
	gp := l.pop()
	assert gp.id == 20

	gp2 := l.pop()
	assert gp2.id == 10
	assert l.empty()
}

// Test WaitQ operations
fn test_waitq() {
	mut q := WaitQ{}
	assert q.empty()

	mut g1 := &Goroutine{
		id: 1
	}
	mut g2 := &Goroutine{
		id: 2
	}

	mut s1 := &Sudog{
		g: g1
	}
	mut s2 := &Sudog{
		g: g2
	}

	q.enqueue(s1)
	assert !q.empty()

	q.enqueue(s2)

	sg := q.dequeue()
	assert sg.g.id == 1

	sg2 := q.dequeue()
	assert sg2.g.id == 2

	assert q.empty()
}

// Test scheduler initialization
fn test_scheduler_init() {
	// The scheduler should have been initialized by module init()
	assert gomaxprocs > 0
	assert gsched.allp.len > 0
	assert gsched.allp.len == int(gomaxprocs)

	// Main never schedules goroutines itself. Workers can use every P, including
	// P0 on a single-core machine, while main blocks waiting for a channel.
	assert get_current_p() == unsafe { nil }
	assert is_main_g(get_current_g())
}

// Test goroutine ID allocation
fn test_goid_allocation() {
	id1 := assign_goid()
	id2 := assign_goid()
	// IDs should be unique and increasing
	assert id2 > id1
}

fn delayed_sender(arg voidptr) {
	time.sleep(100 * time.millisecond)
	c := unsafe { &Chan(arg) }
	mut val := 42
	chan_send(c, voidptr(&val), true)
}

// Test a blocking receive on the main thread: it has no scheduler context to switch to,
// so it must block the thread until the sender wakes it.
fn test_main_thread_blocking_recv() {
	c := chan_make(int(sizeof(int)), 0)
	goroutine_create(voidptr(delayed_sender), voidptr(c), 0)
	mut got := 0
	received, ok := chan_recv(c, voidptr(&got), true)
	assert received
	assert ok
	assert got == 42
	gp := get_current_g()
	assert is_main_g(gp)
	assert gp.status == .running
}

struct DelayedRecv {
mut:
	c   &Chan = unsafe { nil }
	got int
}

fn delayed_receiver(arg voidptr) {
	time.sleep(100 * time.millisecond)
	mut r := unsafe { &DelayedRecv(arg) }
	chan_recv(r.c, voidptr(&r.got), true)
}

// Test a blocking send on the main thread, completed by a goroutine that receives later.
fn test_main_thread_blocking_send() {
	mut r := &DelayedRecv{
		c: chan_make(int(sizeof(int)), 0)
	}
	goroutine_create(voidptr(delayed_receiver), voidptr(r), 0)
	mut val := 42
	assert chan_send(r.c, voidptr(&val), true)
	// The receiver copies the value before it wakes the main thread.
	assert r.got == 42
	gp := get_current_g()
	assert is_main_g(gp)
	assert gp.status == .running
}

// Test that gosched on the main thread returns and keeps the main goroutine current.
fn test_main_thread_gosched() {
	gosched()
	gp := get_current_g()
	assert is_main_g(gp)
	assert gp.status == .running
}

const chain_length = 1000

struct Chain {
mut:
	ran  int
	done &Chan = unsafe { nil }
}

// chain_link creates the next link before it exits. The next link usually runs on the
// same M after this one exited, so it creates its own successor from the dead G of this
// one: this repeats the exit/reuse cycle of goroutine contexts (fibers on Windows).
fn chain_link(arg voidptr) {
	mut st := unsafe { &Chain(arg) }
	st.ran++
	if st.ran < chain_length {
		goroutine_create(voidptr(chain_link), arg, 0)
		return
	}
	mut n := st.ran
	chan_send(st.done, voidptr(&n), false)
}

// Test many goroutine lifetimes, with dead goroutines reused by goroutines.
fn test_goroutine_reuse_chain() {
	allgs_mu.acquire()
	first := allgs.len
	allgs_mu.release()
	mut st := &Chain{
		done: chan_make(int(sizeof(int)), 1)
	}
	goroutine_create(voidptr(chain_link), voidptr(st), 0)
	mut n := 0
	for _ in 0 .. 1000 {
		received, _ := chan_recv(st.done, voidptr(&n), false)
		if received {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert n == chain_length
	// Every link is recorded in allgs; reused goroutines appear there more than once.
	mut distinct := map[u64]bool{}
	allgs_mu.acquire()
	assert allgs.len - first == chain_length
	for i in first .. allgs.len {
		distinct[u64(voidptr(allgs[i]))] = true
	}
	allgs_mu.release()
	assert distinct.len < chain_length / 2
}
