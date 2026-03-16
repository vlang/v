// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// GMP Scheduler - translated from Go's runtime/proc.go.
//
// The scheduler distributes ready-to-run goroutines over worker threads.
// Key functions translated from Go:
//   - schedule()      -> scheduler_loop()
//   - findRunnable()  -> find_runnable()
//   - execute()       -> execute()
//   - newproc()       -> goroutine_create()
//   - wakep()         -> wake_p()
//   - runqput()       -> runq_put()
//   - runqget()       -> runq_get()
//   - stealWork()     -> steal_work()
module goroutines

import sync
import runtime

// goroutine_create creates a new goroutine to run `f` with argument `arg`.
// This is the equivalent of Go's `newproc` function.
// Called by the compiler for `go expr()`.
pub fn goroutine_create(f voidptr, arg voidptr, arg_size int) {
	gp := newproc1(f, arg, arg_size)
	// Get the current P and put the new G on its local run queue
	pp := get_current_p()
	if pp != unsafe { nil } {
		runq_put(mut pp, gp, true)
	} else {
		// No P available, put on global queue
		gsched.mu.@lock()
		gsched.runq.push_back(gp)
		gsched.mu.unlock()
	}
	// Try to wake an idle P if needed
	wake_p()
}

// newproc1 allocates and initializes a new G.
// Translated from Go's newproc1 in proc.go.
fn newproc1(f voidptr, arg voidptr, arg_size int) &Goroutine {
	// Try to get a dead G from the local P's free list first
	pp := get_current_p()
	mut gp := if pp != unsafe { nil } {
		gfget(mut pp)
	} else {
		unsafe { nil }
	}
	if gp == unsafe { nil } {
		// Try global free list
		gp = gfget_global()
	}
	if gp == unsafe { nil } {
		// Allocate a new G
		gp = &Goroutine{}
	}

	// Allocate or reuse stack
	if gp.stack == unsafe { nil } {
		stack_size := goroutines.default_stack_size
		gp.stack = unsafe { malloc(stack_size) }
		gp.stack_size = stack_size
	}

	gp.fn_ptr = f
	gp.fn_arg = arg
	gp.status = .runnable
	gp.preempt = false

	// Assign goroutine ID
	gp.id = assign_goid()

	// Initialize context to run goroutine_entry
	context_init(mut &gp.context, gp.stack, gp.stack_size, goroutine_trampoline, voidptr(gp))

	// Track all goroutines
	allgs_mu.@lock()
	allgs << gp
	allgs_mu.unlock()

	return gp
}

// goroutine_trampoline is the entry point for all goroutines.
// It calls the user function, then cleans up and returns to the scheduler.
// Equivalent to Go's goexit -> goexit0.
fn goroutine_trampoline(arg voidptr) {
	mut gp := unsafe { &Goroutine(arg) }

	// Call the actual function
	if gp.fn_ptr != unsafe { nil } {
		f := unsafe { *(&fn (voidptr)(gp.fn_ptr)) }
		f(gp.fn_arg)
	}

	// Function returned - goroutine is done
	goexit0(mut gp)
}

// goexit0 handles goroutine cleanup after the user function returns.
// Translated from Go's goexit0 in proc.go.
fn goexit0(mut gp Goroutine) {
	gp.status = .dead
	gp.m = unsafe { nil }
	gp.fn_ptr = unsafe { nil }
	gp.fn_arg = unsafe { nil }
	gp.wait_reason = ''
	gp.preempt = false

	// Put the dead G on the free list for reuse
	pp := get_current_p()
	if pp != unsafe { nil } {
		gfput(mut pp, gp)
	} else {
		gfput_global(gp)
	}

	// Return to the scheduler to find more work
	schedule()
}

// schedule is the main scheduler entry point.
// Finds a runnable goroutine and executes it. Never returns.
// Translated from Go's schedule() in proc.go.
pub fn schedule() {
	mut mp := get_current_m()
	if mp == unsafe { nil } {
		return
	}
	mut pp := mp.p
	if pp == unsafe { nil } {
		return
	}
	pp.sched_tick++

	// Find a runnable goroutine
	gp, _ := find_runnable(mut mp, mut pp)
	if gp == unsafe { nil } {
		// No work found - park this M
		park_m(mut mp)
		return
	}

	// Execute the goroutine
	execute(mut mp, mut pp, gp)
}

// find_runnable finds a runnable goroutine to execute.
// Tries: local queue, global queue, work stealing from other P's.
// Translated from Go's findRunnable() in proc.go.
fn find_runnable(mut mp Machine, mut pp Processor) (&Goroutine, bool) {
	// Check global queue every Nth tick for fairness (Go uses 61)
	if pp.sched_tick % goroutines.global_queue_check_interval == 0 {
		gp := glob_runq_get()
		if gp != unsafe { nil } {
			return gp, false
		}
	}

	// Try local run queue first
	gp, inherit := runq_get(mut pp)
	if gp != unsafe { nil } {
		return gp, inherit
	}

	// Try global run queue
	gp2 := glob_runq_get()
	if gp2 != unsafe { nil } {
		return gp2, false
	}

	// Try to steal work from other P's
	gp3 := steal_work(pp)
	if gp3 != unsafe { nil } {
		return gp3, false
	}

	return unsafe { nil }, false
}

// execute runs a goroutine on the current M.
// Translated from Go's execute() in proc.go.
fn execute(mut mp Machine, mut pp Processor, gp &Goroutine) {
	mut g := unsafe { gp }
	// Associate G with M
	mp.curg = g
	g.m = mp
	g.status = .running

	// Switch context to the goroutine
	context_switch(mut &mp.g0.context, &g.context)

	// When we return here, the goroutine has yielded back to us
	// The scheduler loop will be re-entered
}

// wake_p tries to wake an idle P to run goroutines.
// Translated from Go's wakep() in proc.go.
fn wake_p() {
	// Check if there's an idle P
	gsched.mu.@lock()
	if gsched.npidle == 0 {
		gsched.mu.unlock()
		return
	}
	// Don't wake if there are already spinning M's
	if gsched.nmspinning > 0 {
		gsched.mu.unlock()
		return
	}
	// Get an idle P
	pp := pid_get()
	if pp == unsafe { nil } {
		gsched.mu.unlock()
		return
	}
	gsched.mu.unlock()

	// Start a new M for this P (or wake an idle one)
	start_m(pp)
}

// park_m parks the current M - it goes to sleep waiting for work.
fn park_m(mut mp Machine) {
	// Release P
	if mp.p != unsafe { nil } {
		pid_put(mp.p)
		mp.p = unsafe { nil }
	}

	// Add M to idle list
	gsched.mu.@lock()
	mp.sched_link = gsched.midle
	gsched.midle = mp
	gsched.nmidle++
	gsched.mu.unlock()

	// Sleep until woken
	mp.park.wait()

	// Woken up - acquire a P and continue scheduling
	acquire_p(mut mp)
	schedule()
}

// acquire_p tries to get an idle P for the given M.
fn acquire_p(mut mp Machine) {
	gsched.mu.@lock()
	pp := pid_get()
	gsched.mu.unlock()
	if pp != unsafe { nil } {
		wire_p(mut mp, mut pp)
	}
}

// wire_p associates a P with an M.
fn wire_p(mut mp Machine, mut pp Processor) {
	mp.p = pp
	pp.m = mp
	pp.status = .running
}

// start_m starts or wakes an M to run the given P.
// Translated from Go's startm() in proc.go.
fn start_m(pp &Processor) {
	gsched.mu.@lock()

	// Try to get an idle M first
	mut mp := gsched.midle
	if mp != unsafe { nil } {
		gsched.midle = mp.sched_link
		gsched.nmidle--
		gsched.mu.unlock()
		// Give it the P and wake it
		mut p := unsafe { pp }
		wire_p(mut mp, mut p)
		mp.park.post()
		return
	}

	// No idle M available - create a new one
	id := gsched.mnext
	gsched.mnext++
	gsched.mu.unlock()

	new_m(id, pp)
}

// new_m creates a new OS thread (M) and associates it with a P.
fn new_m(id i64, pp &Processor) {
	mut mp := &Machine{
		id: id
		g0: &Goroutine{
			status: .running
		}
	}
	mut p := unsafe { pp }
	wire_p(mut mp, mut p)
	mp.thread = spawn m_thread_entry(mut mp)
}

// m_thread_entry is the entry point for new M (OS thread) goroutine scheduling loops.
fn m_thread_entry(mut mp Machine) {
	// Enter the scheduling loop - this never returns
	schedule_loop(mut mp)
}

// schedule_loop is the main loop for an M. It repeatedly finds and runs goroutines.
fn schedule_loop(mut mp Machine) {
	for {
		if gsched.stopped {
			return
		}
		mut pp := mp.p
		if pp == unsafe { nil } {
			acquire_p(mut mp)
			pp = mp.p
			if pp == unsafe { nil } {
				// No P available, park
				park_m(mut mp)
				continue
			}
		}
		pp.sched_tick++

		gp, _ := find_runnable(mut mp, mut pp)
		if gp == unsafe { nil } {
			// No work - try spinning briefly before parking
			if !mp.spinning {
				mp.spinning = true
				C.atomic_fetch_add(&gsched.nmspinning, 1)
			}
			// Spin a bit
			mut found := false
			for _ in 0 .. 20 {
				gp2, _ := find_runnable(mut mp, mut pp)
				if gp2 != unsafe { nil } {
					mp.spinning = false
					C.atomic_fetch_sub(&gsched.nmspinning, 1)
					execute(mut mp, mut pp, gp2)
					found = true
					break
				}
				// Yield to avoid burning CPU
				proc_yield(10)
			}
			if !found {
				if mp.spinning {
					mp.spinning = false
					C.atomic_fetch_sub(&gsched.nmspinning, 1)
				}
				park_m(mut mp)
			}
			continue
		}

		if mp.spinning {
			mp.spinning = false
			C.atomic_fetch_sub(&gsched.nmspinning, 1)
		}

		execute(mut mp, mut pp, gp)
	}
}

// steal_work tries to steal goroutines from other P's run queues.
// Translated from Go's stealWork() in proc.go.
fn steal_work(thisp &Processor) &Goroutine {
	n := gsched.allp.len
	if n <= 1 {
		return unsafe { nil }
	}
	// Randomize starting point to avoid contention
	start := u32(C.rand()) % u32(n)
	for i := u32(0); i < u32(n); i++ {
		idx := (start + i) % u32(n)
		pp := gsched.allp[idx]
		if pp == thisp || pp.status != .running {
			continue
		}
		// Try to steal half of the target's run queue
		gp := runq_steal(mut unsafe { pp }, thisp)
		if gp != unsafe { nil } {
			return gp
		}
	}
	return unsafe { nil }
}

// runq_steal steals half of pp's local run queue.
// Translated from Go's runqgrab/runqsteal in proc.go.
fn runq_steal(mut pp Processor, thisp &Processor) &Goroutine {
	t := C.atomic_load(&pp.runq_tail)
	h := C.atomic_load(&pp.runq_head)
	n := t - h
	if n == 0 {
		// Try runnext
		next := pp.runnext
		if next != unsafe { nil } {
			if unsafe { C.atomic_compare_exchange_strong(&pp.runnext, &next, nil) } {
				return next
			}
		}
		return unsafe { nil }
	}
	// Steal half
	steal := n - n / 2
	mut first := unsafe { &Goroutine(nil) }
	for i := u32(0); i < steal; i++ {
		gp := pp.runq[(h + i) % goroutines.local_queue_size]
		if i == 0 {
			first = gp
		}
	}
	C.atomic_fetch_add(&pp.runq_head, steal)
	return first
}

// Global run queue operations (translated from Go's globrunqput/get)
fn glob_runq_put(gp &Goroutine) {
	gsched.mu.@lock()
	gsched.runq.push_back(gp)
	gsched.mu.unlock()
}

fn glob_runq_get() &Goroutine {
	gsched.mu.@lock()
	gp := gsched.runq.pop()
	gsched.mu.unlock()
	return gp
}

// Local run queue operations (translated from Go's runqput/runqget)

// runq_put puts a G on the local run queue.
// If next is true, it goes into runnext for immediate scheduling.
// Translated from Go's runqput() in proc.go.
fn runq_put(mut pp Processor, gp &Goroutine, next bool) {
	if next {
		// Fast path: put as runnext
		old := pp.runnext
		pp.runnext = unsafe { gp }
		if old == unsafe { nil } {
			return
		}
		// Kick old runnext to the regular queue
		runq_put(mut pp, old, false)
		return
	}

	// Regular path: put on the ring buffer
	h := C.atomic_load(&pp.runq_head)
	t := pp.runq_tail
	if t - h < goroutines.local_queue_size {
		pp.runq[t % goroutines.local_queue_size] = unsafe { gp }
		C.atomic_store(&pp.runq_tail, t + 1)
		return
	}
	// Queue is full - put half on global queue
	runq_put_slow(mut pp, gp, h, t)
}

// runq_put_slow moves half the local queue to the global queue.
// Translated from Go's runqputslow() in proc.go.
fn runq_put_slow(mut pp Processor, gp &Goroutine, h u32, t u32) {
	n := (t - h) / 2
	mut batch := GoroutineQueue{}
	for i := u32(0); i < n; i++ {
		g := pp.runq[(h + i) % goroutines.local_queue_size]
		batch.push_back(g)
	}
	C.atomic_fetch_add(&pp.runq_head, n)
	batch.push_back(gp)

	gsched.mu.@lock()
	for !batch.empty() {
		gsched.runq.push_back(batch.pop())
	}
	gsched.mu.unlock()
}

// runq_get gets a G from the local run queue.
// Translated from Go's runqget() in proc.go.
fn runq_get(mut pp Processor) (&Goroutine, bool) {
	// Check runnext first (fast path)
	next := pp.runnext
	if next != unsafe { nil } {
		pp.runnext = unsafe { nil }
		return next, true
	}

	// Regular queue
	for {
		h := C.atomic_load(&pp.runq_head)
		t := pp.runq_tail
		if t == h {
			return unsafe { nil }, false
		}
		gp := pp.runq[h % goroutines.local_queue_size]
		if unsafe { C.atomic_compare_exchange_strong(&pp.runq_head, &h, h + 1) } {
			return gp, false
		}
	}
	return unsafe { nil }, false
}

// Idle P list operations
fn pid_get() &Processor {
	pp := gsched.pidle
	if pp != unsafe { nil } {
		gsched.pidle = pp.link
		gsched.npidle--
		mut p := unsafe { pp }
		p.status = .running
		p.link = unsafe { nil }
	}
	return pp
}

fn pid_put(pp &Processor) {
	mut p := unsafe { pp }
	p.status = .idle
	p.m = unsafe { nil }
	p.link = gsched.pidle
	gsched.pidle = p
	gsched.npidle++
}

// G free list operations (translated from Go's gfput/gfget)
fn gfput(mut pp Processor, gp &Goroutine) {
	mut g := unsafe { gp }
	g.status = .dead
	g.fn_ptr = unsafe { nil }
	g.fn_arg = unsafe { nil }
	g.sched_link = unsafe { nil }
	pp.g_free.push(g)
}

fn gfget(mut pp Processor) &Goroutine {
	return pp.g_free.pop()
}

fn gfput_global(gp &Goroutine) {
	gsched.g_free_mu.@lock()
	gsched.g_free.push(unsafe { gp })
	gsched.g_free_count++
	gsched.g_free_mu.unlock()
}

fn gfget_global() &Goroutine {
	gsched.g_free_mu.@lock()
	gp := gsched.g_free.pop()
	if gp != unsafe { nil } {
		gsched.g_free_count--
	}
	gsched.g_free_mu.unlock()
	return gp
}

// assign_goid allocates a unique goroutine ID.
// Uses per-P caching to avoid contention (like Go's goidcache).
fn assign_goid() u64 {
	pp := get_current_p()
	if pp != unsafe { nil } && pp.goid_cache < pp.goid_cache_end {
		id := pp.goid_cache
		unsafe {
			pp.goid_cache++
		}
		return id
	}
	// Refill cache from global counter
	batch := u64(16)
	id := C.atomic_fetch_add(&gsched.goid_gen, batch)
	if pp != unsafe { nil } {
		unsafe {
			pp.goid_cache = id + 1
			pp.goid_cache_end = id + batch
		}
	}
	return id
}

// proc_yield spins for a short time (used during work stealing).
fn proc_yield(count int) {
	for _ in 0 .. count {
		// CPU pause instruction to reduce power and contention
		$if amd64 {
			unsafe { asm volatile { `pause` } }
		}
		$if arm64 {
			unsafe { asm volatile { `yield` } }
		}
	}
}

// get_current_m returns the M for the current OS thread.
// Uses thread-local storage.
__global current_m = thread_local &Machine(unsafe { nil })

fn get_current_m() &Machine {
	return current_m
}

fn set_current_m(mp &Machine) {
	current_m = unsafe { mp }
}

// get_current_p returns the P for the current OS thread's M.
fn get_current_p() &Processor {
	mp := get_current_m()
	if mp == unsafe { nil } {
		return unsafe { nil }
	}
	return mp.p
}

// get_current_g returns the currently running G.
pub fn get_current_g() &Goroutine {
	mp := get_current_m()
	if mp == unsafe { nil } {
		return unsafe { nil }
	}
	return mp.curg
}
