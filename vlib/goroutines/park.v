// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Goroutine parking (blocking/unblocking) mechanism.
// Translated from Go's gopark/goready in proc.go.
//
// When a goroutine needs to block (e.g., waiting on a channel),
// it "parks" itself - yielding its M to the scheduler.
// When the condition is met, another goroutine "readies" it
// by putting it back on a run queue.
module goroutines

// gopark puts the current goroutine into a waiting state.
// The goroutine can be made runnable again by calling goready.
// reason describes why the goroutine is parking (for debugging).
// Translated from Go's gopark() in proc.go.
pub fn gopark(reason string) {
	mut mp := get_current_m()
	if mp == unsafe { nil } {
		return
	}
	mut gp := mp.curg
	if gp == unsafe { nil } {
		return
	}

	gp.status = .waiting
	gp.wait_reason = reason

	// Dissociate G from M
	mp.curg = unsafe { nil }
	gp.m = unsafe { nil }

	// Switch back to the scheduler (M's g0 context)
	context_switch(mut &gp.context, &mp.g0.context)
}

// goready puts a waiting goroutine back on a run queue.
// Translated from Go's goready() in proc.go.
pub fn goready(gp &Goroutine) {
	mut g := unsafe { gp }
	if g.status != .waiting {
		return
	}
	g.status = .runnable
	g.wait_reason = ''

	// Put it on the current P's local run queue, or global if no P
	pp := get_current_p()
	if pp != unsafe { nil } {
		runq_put(mut pp, g, true)
	} else {
		glob_runq_put(g)
	}

	// Try to wake an idle P to run this goroutine
	wake_p()
}

// gosched yields the processor, allowing other goroutines to run.
// Translated from Go's Gosched() / goschedImpl() in proc.go.
pub fn gosched() {
	mut mp := get_current_m()
	if mp == unsafe { nil } {
		return
	}
	mut gp := mp.curg
	if gp == unsafe { nil } {
		return
	}
	mut pp := mp.p
	if pp == unsafe { nil } {
		return
	}

	// Put the current G back on the run queue as runnable
	gp.status = .runnable
	mp.curg = unsafe { nil }
	gp.m = unsafe { nil }

	// Put on local queue
	runq_put(mut pp, gp, false)

	// Switch back to scheduler
	context_switch(mut &gp.context, &mp.g0.context)
}

// Sudog represents a G in a wait list (e.g., channel wait queue).
// Translated from Go's sudog struct in runtime2.go.
pub struct Sudog {
pub mut:
	g       &Goroutine = unsafe { nil }       // the waiting goroutine
	next    &Sudog = unsafe { nil }    // next in wait list
	prev    &Sudog = unsafe { nil }    // prev in wait list
	elem    voidptr                    // data element (may point to stack)
	success bool                       // true if woken by successful channel op
	c       voidptr                    // channel pointer
}

// WaitQ is a wait queue of Sudogs (used by channels).
// Translated from Go's waitq struct in runtime2.go.
pub struct WaitQ {
pub mut:
	first &Sudog = unsafe { nil }
	last  &Sudog = unsafe { nil }
}

pub fn (mut q WaitQ) enqueue(s &Sudog) {
	mut sg := unsafe { s }
	sg.next = unsafe { nil }
	sg.prev = q.last
	if q.last != unsafe { nil } {
		q.last.next = sg
	} else {
		q.first = sg
	}
	q.last = sg
}

pub fn (mut q WaitQ) dequeue() &Sudog {
	sg := q.first
	if sg == unsafe { nil } {
		return unsafe { nil }
	}
	q.first = sg.next
	if q.first != unsafe { nil } {
		q.first.prev = unsafe { nil }
	} else {
		q.last = unsafe { nil }
	}
	mut s := unsafe { sg }
	s.next = unsafe { nil }
	s.prev = unsafe { nil }
	return s
}

pub fn (q &WaitQ) empty() bool {
	return q.first == unsafe { nil }
}
