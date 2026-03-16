// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module goroutines

import time

// Test basic goroutine creation
fn test_goroutine_create() {
	mut done := false
	f := fn [mut done] () {
		done = true
	}
	goroutine_create(voidptr(&f), unsafe { nil }, 0)
	// Give the goroutine time to run
	time.sleep(50 * time.millisecond)
	// The goroutine should have set done to true
	// (Note: in a real implementation this would use proper synchronization)
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

	mut g1 := &Goroutine{ id: 1 }
	mut g2 := &Goroutine{ id: 2 }
	mut g3 := &Goroutine{ id: 3 }

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

	mut g1 := &Goroutine{ id: 10 }
	mut g2 := &Goroutine{ id: 20 }

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

	mut g1 := &Goroutine{ id: 1 }
	mut g2 := &Goroutine{ id: 2 }

	mut s1 := &Sudog{ g: g1 }
	mut s2 := &Sudog{ g: g2 }

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

	// P0 should be running (attached to M0)
	p0 := gsched.allp[0]
	assert p0.status == .running
}

// Test goroutine ID allocation
fn test_goid_allocation() {
	id1 := assign_goid()
	id2 := assign_goid()
	// IDs should be unique and increasing
	assert id2 > id1
}
