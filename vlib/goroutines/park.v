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

// On Windows, the main thread runs user code on a fiber (pump_user) that can
// never switch to the scheduler fiber, because a fiber cannot switch to
// itself. Parking or yielding from main thread user code therefore pumps the
// scheduler by spinning until another thread readies the goroutine, instead
// of switching fibers. Only used on Windows.
__global pump_user = Context{}

// pump_is_user reports whether the calling thread is currently running main
// thread user code. Only true on Windows after init().
fn pump_is_user() bool {
	$if windows {
		return pump_user.uctx != unsafe { nil } && current_fiber() == pump_user.uctx
	} $else {
		return false
	}
}

// pump_scheduler waits until `gp` is readied by another goroutine. Used on
// Windows when parking from main thread user code, which cannot switch
// fibers. It lends its P so a worker M can run the peer goroutine (this
// matters when no other P is idle), wakes a worker, and spins.
fn pump_scheduler(mut mp Machine, gp &Goroutine) {
	mut g := unsafe { gp }
	mut m := unsafe { mp }
	mut had_p := false
	if m.p != unsafe { nil } {
		gsched.mu.acquire()
		pid_put(m.p)
		m.p = unsafe { nil }
		gsched.mu.release()
		had_p = true
	}
	wake_p_no_spawn()
	// Re-wake periodically: a wake that lands while the worker is still
	// spinning (not yet parked) is a no-op, and the worker would otherwise
	// never notice the newly queued peer. Waking an already busy worker is
	// harmless (it simply finds no idle M to reuse).
	mut spins := u32(0)
	for g.status == .waiting {
		if spins % 64 == 0 {
			wake_p_no_spawn()
		}
		spins++
		proc_yield(100)
	}
	if had_p {
		// Reacquire a P; the worker that ran the peer may still hold ours.
		for _ in 0 .. 10000 {
			acquire_p(mut m)
			if m.p != unsafe { nil } {
				break
			}
			proc_yield(100)
		}
	}
}

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

	// On Windows, parking from main thread user code cannot switch fibers.
	// Pump the scheduler by spinning until this goroutine is readied.
	if pump_is_user() {
		pump_scheduler(mut mp, gp)
		// Still the current goroutine: reassociate it before returning.
		mp.curg = gp
		mut g := unsafe { gp }
		g.m = mp
		return
	}

	// Switch back to the scheduler (M's g0 context)
	context_switch(mut &gp.context, &mp.g0.context)
}

// goready puts a waiting goroutine back on a run queue.
// Translated from Go's goready() in proc.go.
pub fn goready(gp &Goroutine) {
	if gp == unsafe { nil } {
		return
	}
	mut g := unsafe { gp }
	if g.status != .waiting {
		return
	}
	g.status = .runnable
	g.wait_reason = ''

	// A pumped goroutine is owned by a thread that pumps the scheduler
	// (see pump_scheduler). The status flip above is its wakeup; never
	// place it on a run queue, where another thread could steal a context
	// it does not own.
	if g.pumped {
		return
	}

	// Put it on the current P's local run queue, or global if no P
	mut pp := get_current_p()
	if pp != unsafe { nil } {
		runq_put(mut pp, g, true)
	} else {
		glob_runq_put(g)
	}

	// Try to wake an idle P to run this goroutine
	wake_p()
}

// park_setup marks gp as pumped when the calling thread will pump the
// scheduler instead of switching fibers (Windows main thread user code).
// Must be called before the goroutine becomes visible to wakers (i.e.
// before enqueueing on a channel wait queue). Returns true when the caller
// must clear the flag after gopark returns.
fn park_setup(gp &Goroutine) bool {
	if pump_is_user() {
		if gp == unsafe { nil } {
			return false
		}
		mut g := unsafe { gp }
		g.pumped = true
		return true
	}
	return false
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

	// On Windows, yielding from main thread user code cannot switch fibers.
	// Wake a worker so peers can progress, yield our timeslice, and return
	// with the yielding goroutine still current.
	if pump_is_user() {
		wake_p_no_spawn()
		proc_yield(100)
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
	g       &Goroutine = unsafe { nil } // the waiting goroutine
	next    &Sudog     = unsafe { nil } // next in wait list
	prev    &Sudog     = unsafe { nil } // prev in wait list
	elem    voidptr // data element (may point to stack)
	success bool    // true if woken by successful channel op
	c       voidptr // channel pointer
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
